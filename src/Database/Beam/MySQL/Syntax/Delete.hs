module Database.Beam.MySQL.Syntax.Delete
    ( MySQLDeleteSyntax (..)
    ) where

import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Expression
import Database.Beam.MySQL.Syntax.TableName
import Database.Beam.MySQL.Syntax.Type

newtype MySQLDeleteSyntax = MySQLDeleteSyntax {fromMySQLDelete :: MySQLSyntax} deriving stock (Eq, Show)

instance IsSql92DeleteSyntax MySQLDeleteSyntax where
    type Sql92DeleteTableNameSyntax MySQLDeleteSyntax = MySQLTableNameSyntax
    type Sql92DeleteExpressionSyntax MySQLDeleteSyntax = MySQLExpressionSyntax

    deleteStmt tblName _aliasMaybe whereMaybe =
        MySQLDeleteSyntax $
            emit "DELETE FROM "
                <> fromMySQLTableName tblName
                <> maybe mempty (\where_ -> emit " WHERE " <> fromMySQLExpression where_) whereMaybe
    deleteSupportsAlias _ = False -- MySQL 8 supports it, but MariaDB doesn't
