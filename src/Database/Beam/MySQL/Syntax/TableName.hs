module Database.Beam.MySQL.Syntax.TableName
    ( MySQLTableNameSyntax (..)
    ) where

import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Type

newtype MySQLTableNameSyntax = MySQLTableNameSyntax {fromMySQLTableName :: MySQLSyntax}

instance IsSql92TableNameSyntax MySQLTableNameSyntax where
    tableName Nothing t = MySQLTableNameSyntax $ quotedIdentifier t
    tableName (Just db) t = MySQLTableNameSyntax $ quotedIdentifier db <> emit "." <> quotedIdentifier t
