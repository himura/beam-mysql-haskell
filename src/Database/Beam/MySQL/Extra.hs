{-# LANGUAGE NoFieldSelectors #-}

module Database.Beam.MySQL.Extra (runInsertLastRowReturning) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.Beam
import Database.Beam.MySQL.Connection
import Database.Beam.MySQL.MySQLSpecific
import Database.Beam.MySQL.Syntax
import Database.Beam.MySQL.Syntax.Insert
import Database.Beam.MySQL.Syntax.TableName
import Database.Beam.MySQL.Syntax.Type
import Database.Beam.MySQL.Table.InformationSchema
import Database.Beam.Schema.Tables
import UnliftIO.Exception

data BeamMySQLFailedToObtainAutoIncrementColumn = BeamMySQLFailedToObtainAutoIncrementColumn
    { schemaName :: Maybe T.Text
    , tableName :: T.Text
    }
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

runInsertLastRowReturning
    :: (Beamable table, FromBackendRow MySQL (table Identity))
    => SqlInsert MySQL table
    -> MySQLM (Maybe (table Identity))
runInsertLastRowReturning SqlInsertNoRows =
    return Nothing
runInsertLastRowReturning ins@(SqlInsert tableSettings (MySQLInsertSyntax tblName _)) = do
    detectAutoIncrementColumn tblName >>= \case
        Nothing ->
            throwIO $ BeamMySQLFailedToObtainAutoIncrementColumn (schemaName tblName) (table tblName)
        Just aiColumnName -> do
            runInsert ins

            let columns = allBeamValues (\(Columnar' fieldProj) -> quotedIdentifier (_fieldName fieldProj)) tableSettings
                query =
                    mconcat
                        [ emit "SELECT "
                        , commas columns
                        , emit " FROM "
                        , fromMySQLTableName tblName
                        , emit " WHERE "
                        , emit (T.encodeUtf8 aiColumnName)
                        , emit " = last_insert_id()"
                        ]
            runReturningOne (MySQLCommandQuery query)

detectAutoIncrementColumn
    :: MySQLTableNameSyntax
    -> MySQLM (Maybe T.Text)
detectAutoIncrementColumn tblName =
    runSelectReturningOne $ select $ do
        columns <- all_ informationSchemaDb.tableColumns
        guard_ $ columns.tableSchema ==. maybe schema_ val_ (schemaName tblName) &&. columns.tableName ==. val_ (table tblName) &&. columns.extra `like_` "%auto_increment%"
        return columns.columnName
