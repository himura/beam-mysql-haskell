{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Database.Beam.MySQL.Logger (LogEntry (..), formatLogSimple) where

import Data.Binary.Put (runPut)
import Data.ByteString.Builder qualified as BSBuilder
import Data.ByteString.Lazy qualified as L
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Encoding qualified as TL
import Database.Beam.MySQL.Syntax (MySQLCommandSyntax (..))
import Database.Beam.MySQL.Syntax.Type (MySQLSyntax (..), SqlBuilder (..))
import Database.MySQL.Base (ColumnDef, OK (..), StmtPrepareOK (..))
import Database.MySQL.Base qualified as MySQL

type PrepareOk = (StmtPrepareOK, [ColumnDef], [ColumnDef])

data LogEntry
    = PrepareStatement
        { query :: MySQLCommandSyntax
        , renderedQuery :: L.ByteString
        }
    | PrepareStatementOK
        { query :: MySQLCommandSyntax
        , renderedQuery :: L.ByteString
        , prepareOk :: PrepareOk
        }
    | QueryStatement
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        }
    | QueryStatementOK
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        , columnDefs :: [ColumnDef]
        }
    | ExecuteStatement
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        }
    | ExecuteStatementOK
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        , response :: OK
        }
    deriving stock (Show)

buildSqlForLog :: SqlBuilder -> Builder.Builder
buildSqlForLog (SqlBuilder builder) = Builder.fromLazyText . TL.decodeUtf8 . BSBuilder.toLazyByteString $ builder renderParam
  where
    renderParam = BSBuilder.lazyByteString . runPut . MySQL.putTextField

formatLogSimple :: LogEntry -> Maybe TL.Text
formatLogSimple =
    fmap Builder.toLazyText . \case
        PrepareStatement{} -> Nothing
        PrepareStatementOK _ rquery (StmtPrepareOK{..}, _, _) ->
            Just $
                mconcat
                    [ "Prepare: stmtID="
                    , showB stmtId
                    , ", columns="
                    , showB stmtColumnCnt
                    , ", params="
                    , showB stmtParamCnt
                    , ", warns="
                    , showB stmtWarnCnt
                    , ": "
                    , Builder.fromLazyText (TL.decodeUtf8 rquery)
                    ]
        QueryStatement{} -> Nothing
        QueryStatementOK (MySQLCommandQuery (MySQLSyntax query _)) (StmtPrepareOK{stmtId}, _, _) _ ->
            Just $
                mconcat
                    [ "Query: stmtID="
                    , showB stmtId
                    , ": "
                    , buildSqlForLog query
                    ]
        ExecuteStatement{} -> Nothing
        ExecuteStatementOK (MySQLCommandQuery (MySQLSyntax query _)) (StmtPrepareOK{stmtId}, _, _) OK{..} ->
            Just $
                mconcat
                    [ "Execute: stmtID="
                    , showB stmtId
                    , ", affectedRows="
                    , showB okAffectedRows
                    , ", lastInsertID="
                    , showB okLastInsertID
                    , ", status="
                    , showB okStatus
                    , ", warningCnt="
                    , showB okWarningCnt
                    , ": "
                    , buildSqlForLog query
                    ]
  where
    showB :: forall a. (Show a) => a -> Builder.Builder
    showB = Builder.fromString . show
