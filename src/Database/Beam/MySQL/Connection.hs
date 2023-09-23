{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.MySQL.Connection
    ( MySQL (..)
    , MySQLM
    , runBeamMySQLMWithDebug
    , runBeamMySQLM
    ) where

import Control.Monad.Free.Church
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.DList qualified as DList
import Data.Text (Text)
import Data.Text qualified as T
import Database.Beam
import Database.Beam.Backend
import Database.Beam.MySQL.Syntax
import Database.Beam.MySQL.Syntax.Type
import Database.MySQL.Base (MySQLConn, MySQLValue)
import Database.MySQL.Base qualified as MySQL
import System.IO.Streams qualified as Streams
import UnliftIO.Exception

data MySQL = MySQL

data MySQLEnv = MySQLEnv
    { connection :: MySQLConn
    , logger :: Text -> IO ()
    }

newtype MySQLM a = MySQLM
    { runMySQLM :: ReaderT MySQLEnv IO a
    }
    deriving newtype (Monad, Functor, Applicative, MonadIO, MonadFail, MonadUnliftIO, MonadReader MySQLEnv) -- MonadBase IO, MonadBaseControl IO, )

runBeamMySQLMWithDebug
    :: (Text -> IO ())
    -- ^ logger
    -> MySQLConn
    -> MySQLM a
    -> IO a
runBeamMySQLMWithDebug logger connection act = do
    let env = MySQLEnv{..}
    runReaderT (runMySQLM act) env

runBeamMySQLM
    :: MySQLConn
    -> MySQLM a
    -> IO a
runBeamMySQLM = runBeamMySQLMWithDebug (const (return ()))

class FromBackendField a where
    fromBackendField :: MySQLValue -> Either Text a

instance BeamBackend MySQL where
    type BackendFromField MySQL = FromBackendField

type instance BeamSqlBackendSyntax MySQL = MySQLCommandSyntax

instance (MonadUnliftIO m, MonadReader MySQLEnv m) => MonadBeam MySQL m where
    runReturningMany (MySQLCommandQuery (MySQLSyntax cmd paramsDL)) action = do
        env <- ask
        let query = MySQL.Query $ buildSqlWithPlaceholder cmd
            params = DList.toList paramsDL
        bracket
            (liftIO $ MySQL.prepareStmtDetail env.connection query)
            (\(ok, _, _) -> liftIO $ MySQL.closeStmt env.connection $ MySQL.stmtId ok)
            $ \(MySQL.StmtPrepareOK{stmtId}, _pcols, _rcols) ->
                bracket
                    (liftIO $ MySQL.queryStmt env.connection stmtId params)
                    (\(_, istream) -> liftIO $ consume_ istream)
                    $ \(colDefs, istream) -> action (liftIO $ readRow colDefs istream)
    runNoReturn (MySQLCommandQuery (MySQLSyntax cmd paramsDL)) = do
        env <- ask
        let query = MySQL.Query $ buildSqlWithPlaceholder cmd
            params = DList.toList paramsDL
        bracket
            (liftIO $ MySQL.prepareStmtDetail env.connection query)
            (\(ok, _, _) -> liftIO $ MySQL.closeStmt env.connection $ MySQL.stmtId ok)
            $ \(MySQL.StmtPrepareOK{stmtId}, _pcols, _rcols) -> do
                liftIO . void $ MySQL.executeStmt env.connection stmtId params

readRow :: (FromBackendRow MySQL a) => [MySQL.ColumnDef] -> Streams.InputStream [MySQLValue] -> IO (Maybe a)
readRow cdefs istream = do
    Streams.read istream >>= \case
        Nothing -> pure Nothing
        Just rows -> case fromRow fromBackendRow cdefs rows of
            Left err -> throwIO err
            Right ok -> return $ Just ok

consume_ :: Streams.InputStream a -> IO ()
consume_ istream = loop
  where
    loop =
        Streams.read istream >>= \case
            Nothing -> return ()
            Just _ -> loop

fromRow :: FromBackendRowM MySQL a -> [MySQL.ColumnDef] -> [MySQLValue] -> Either BeamRowReadError a
fromRow (FromBackendRowM fromBackendRowF) =
    runF fromBackendRowF finish step 0
  where
    finish a _ _ _ = Right a
    -- finish _ _ xs = Left $ BeamRowReadError Nothing {- TODO -} $ ColumnErrorInternal ?

    step
        :: forall x
         . FromBackendRowF MySQL (Int -> [MySQL.ColumnDef] -> [MySQLValue] -> Either BeamRowReadError x)
        -> Int
        -> [MySQL.ColumnDef]
        -> [MySQLValue]
        -> Either BeamRowReadError x
    step (ParseOneField _) curIdx [] _ = Left $ BeamRowReadError (Just curIdx) $ ColumnNotEnoughColumns curIdx
    step (ParseOneField _) curIdx _ [] = Left $ BeamRowReadError (Just curIdx) $ ColumnNotEnoughColumns curIdx
    step (ParseOneField next) curIdx (cdef : cdefs) (v : vs) =
        case fromBackendField v of
            Left err -> Left $ BeamRowReadError (Just curIdx) $ ColumnTypeMismatch "" (show $ MySQL.columnType cdef) $ T.unpack $ "failed to convert " <> err
            Right pv ->
                next pv (curIdx + 1) cdefs vs
    step (FailParseWith err) _ _ _ = Left err
    step (Alt (FromBackendRowM a) (FromBackendRowM b) next) curCol cdefs vs =
        case runF a (chain next) step curCol cdefs vs of
            Right next' -> next'
            Left aErr ->
                case runF b (chain next) step curCol cdefs vs of
                    Right next' -> next'
                    Left _bErr -> Left aErr

    chain next x curIdx cdefs vs = Right $ next x curIdx cdefs vs
