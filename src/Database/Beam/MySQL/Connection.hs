{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.MySQL.Connection
    ( MySQL (..)
    , MySQLM (..)
    , MySQLEnv (..)
    , runBeamMySQLMWithDebug
    , runBeamMySQLM
    , runNoReturn'
    ) where

import Control.Monad (void)
import Control.Monad.Free.Church (F (runF))
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Data.DList qualified as DList
import Data.Typeable (tyConName)
import Database.Beam
    ( FromBackendRow (fromBackendRow)
    , MonadBeam (runNoReturn, runReturningMany)
    )
import Database.Beam.Backend
    ( BeamRowReadError (BeamRowReadError)
    , ColumnParseError (..)
    , FromBackendRowF (..)
    , FromBackendRowM (..)
    )
import Database.Beam.MySQL.Backend (MySQL (..))
import Database.Beam.MySQL.FromField
    ( DecodeError (DecodeError)
    , DecodeErrorDetail (DecodeErrorUnexpectedNull)
    , FromField (fromField)
    )
import Database.Beam.MySQL.Logger
import Database.Beam.MySQL.Syntax
    ( MySQLCommandSyntax (MySQLCommandQuery)
    )
import Database.Beam.MySQL.Syntax.Type
    ( MySQLSyntax (MySQLSyntax)
    , buildSqlWithPlaceholder
    )
import Database.MySQL.Base (MySQLConn, MySQLValue)
import Database.MySQL.Base qualified as MySQL
import System.IO.Streams qualified as Streams
import UnliftIO.Exception (bracket, throwIO)

data MySQLEnv = MySQLEnv
    { connection :: MySQLConn
    , logger :: LogEntry -> IO ()
    }

newtype MySQLM a = MySQLM
    { runMySQLM :: ReaderT MySQLEnv IO a
    }
    deriving newtype (Monad, Functor, Applicative, MonadIO, MonadFail, MonadUnliftIO, MonadReader MySQLEnv) -- MonadBase IO, MonadBaseControl IO, )

runBeamMySQLMWithDebug
    :: (LogEntry -> IO ())
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

instance (MonadUnliftIO m, MonadReader MySQLEnv m) => MonadBeam MySQL m where
    runReturningMany = runReturningMany'
    runNoReturn = void . runNoReturn'

readRow :: (FromBackendRow MySQL a) => [MySQL.ColumnDef] -> Streams.InputStream [MySQLValue] -> IO (Maybe a)
readRow cdefs istream = do
    Streams.read istream >>= \case
        Nothing -> pure Nothing
        Just rows -> case fromRow fromBackendRow cdefs rows of
            Left err -> throwIO err
            Right ok -> return $ Just ok

withPreparedStmt
    :: (MonadUnliftIO m, MonadReader MySQLEnv m)
    => MySQLCommandSyntax
    -> ((MySQL.StmtPrepareOK, [MySQL.ColumnDef], [MySQL.ColumnDef]) -> m a)
    -> m a
withPreparedStmt query@(MySQLCommandQuery (MySQLSyntax cmd _)) action = do
    env <- ask
    let rquery = buildSqlWithPlaceholder cmd
    liftIO $ env.logger $ PrepareStatement query rquery
    bracket
        (liftIO $ MySQL.prepareStmtDetail env.connection $ MySQL.Query rquery)
        (\(ok, _, _) -> liftIO $ MySQL.closeStmt env.connection $ MySQL.stmtId ok)
        $ \prepareOk -> do
            liftIO $ env.logger $ PrepareStatementOK query rquery prepareOk
            action prepareOk

runReturningMany'
    :: ( MonadUnliftIO m
       , MonadReader MySQLEnv m
       , FromBackendRow MySQL a
       )
    => MySQLCommandSyntax
    -> (m (Maybe a) -> m b)
    -> m b
runReturningMany' query@(MySQLCommandQuery (MySQLSyntax _ paramsDL)) action = do
    env <- ask
    withPreparedStmt query $ \prepareOk -> do
        let params = DList.toList paramsDL
        bracket (liftIO $ runQuery env prepareOk params) (\(_, istream) -> liftIO $ Streams.skipToEof istream) $ \(colDefs, istream) -> do
            liftIO $ env.logger $ QueryStatementOK query prepareOk colDefs
            action (liftIO $ readRow colDefs istream)
  where
    runQuery env prepareOk@(MySQL.StmtPrepareOK{stmtId}, _, _) params = do
        env.logger $ QueryStatement query prepareOk
        MySQL.queryStmt env.connection stmtId params

runNoReturn' :: (MonadUnliftIO m, MonadReader MySQLEnv m) => MySQLCommandSyntax -> m MySQL.OK
runNoReturn' query@(MySQLCommandQuery (MySQLSyntax _ paramsDL)) = do
    env <- ask
    withPreparedStmt query $ \prepareOk@(MySQL.StmtPrepareOK{stmtId}, _, _) -> do
        let params = DList.toList paramsDL

        liftIO $ env.logger $ ExecuteStatement query prepareOk
        ok <- liftIO $ MySQL.executeStmt env.connection stmtId params
        liftIO $ env.logger $ ExecuteStatementOK query prepareOk ok
        return ok

fromRow :: FromBackendRowM MySQL a -> [MySQL.ColumnDef] -> [MySQLValue] -> Either BeamRowReadError a
fromRow (FromBackendRowM fromBackendRowF) =
    runF fromBackendRowF finish step 0
  where
    finish a _ _ _ = Right a

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
        case fromField v of
            Left (DecodeError _tyCon DecodeErrorUnexpectedNull) -> Left $ BeamRowReadError (Just curIdx) ColumnUnexpectedNull
            Left (DecodeError tyCon err) -> Left $ BeamRowReadError (Just curIdx) $ ColumnTypeMismatch (tyConName tyCon) (show $ MySQL.columnType cdef) $ "Failed to convert " ++ show err
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
