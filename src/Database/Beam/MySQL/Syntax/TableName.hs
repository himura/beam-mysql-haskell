module Database.Beam.MySQL.Syntax.TableName
    ( MySQLTableNameSyntax (..)
    ) where

import Data.Text (Text)
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Type

data MySQLTableNameSyntax = MySQLTableNameSyntax
    { fromMySQLTableName :: MySQLSyntax
    , schemaName :: Maybe Text
    , table :: Text
    }

instance IsSql92TableNameSyntax MySQLTableNameSyntax where
    tableName Nothing t = MySQLTableNameSyntax (quotedIdentifier t) Nothing t
    tableName (Just db) t = MySQLTableNameSyntax (quotedIdentifier db <> emit "." <> quotedIdentifier t) (Just db) t
