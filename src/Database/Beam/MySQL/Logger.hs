{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Database.Beam.MySQL.Logger (LogEntry (..), formatLogSimple) where

import Data.ByteString.Lazy qualified as L
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Encoding qualified as TL
import Database.Beam.MySQL.Syntax (MySQLCommandSyntax)
import Database.MySQL.Base
    ( ColumnDef
    , MySQLValue
    , OK (..)
    , StmtPrepareOK (..)
    )

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
        , params :: [MySQLValue]
        }
    | QueryStatementOK
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        , params :: [MySQLValue]
        , columnDefs :: [ColumnDef]
        }
    | ExecuteStatement
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        , params :: [MySQLValue]
        }
    | ExecuteStatementOK
        { query :: MySQLCommandSyntax
        , prepareOk :: PrepareOk
        , params :: [MySQLValue]
        , response :: OK
        }
    deriving stock (Show)

formatLogSimple :: LogEntry -> Maybe TL.Text
formatLogSimple =
    fmap Builder.toLazyText . \case
        PrepareStatement{} -> Nothing
        PrepareStatementOK _ rquery (StmtPrepareOK{..}, _, _) ->
            Just $
                mconcat
                    [ "Prepare: "
                    , Builder.fromLazyText (TL.decodeUtf8 rquery)
                    , ", stmtId="
                    , showB stmtId
                    , ", columns="
                    , showB stmtColumnCnt
                    , ", params="
                    , showB stmtParamCnt
                    , ", warns="
                    , showB stmtWarnCnt
                    ]
        QueryStatement{} -> Nothing
        QueryStatementOK _ (StmtPrepareOK{stmtId}, _, _) params _ ->
            Just $
                mconcat
                    [ "Query: stmtID="
                    , showB stmtId
                    , ", params="
                    , showB params
                    ]
        ExecuteStatement{} -> Nothing
        ExecuteStatementOK _ (StmtPrepareOK{stmtId}, _, _) params OK{..} ->
            Just $
                mconcat
                    [ "Execute: stmtID="
                    , showB stmtId
                    , ", params="
                    , showB params
                    , ", affectedRows="
                    , showB okAffectedRows
                    , ", lastInsertID="
                    , showB okLastInsertID
                    , ", status="
                    , showB okStatus
                    , ", warningCnt="
                    , showB okWarningCnt
                    ]
  where
    showB :: forall a. (Show a) => a -> Builder.Builder
    showB = Builder.fromString . show
