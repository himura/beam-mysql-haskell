module Database.Beam.MySQL.Syntax.Update
    ( MySQLUpdateSyntax (..)
    ) where

import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Expression
import Database.Beam.MySQL.Syntax.TableName
import Database.Beam.MySQL.Syntax.Type

newtype MySQLUpdateSyntax = MySQLUpdateSyntax {fromMySQLUpdate :: MySQLSyntax}

instance IsSql92UpdateSyntax MySQLUpdateSyntax where
    type Sql92UpdateTableNameSyntax MySQLUpdateSyntax = MySQLTableNameSyntax
    type Sql92UpdateFieldNameSyntax MySQLUpdateSyntax = MySQLFieldNameSyntax
    type Sql92UpdateExpressionSyntax MySQLUpdateSyntax = MySQLExpressionSyntax

    updateStmt tblName fields whereMaybe =
        MySQLUpdateSyntax $
            emit "UPDATE "
                <> fromMySQLTableName tblName
                <> renderFields fields
                <> maybe mempty (\where_ -> emit " WHERE " <> fromMySQLExpression where_) whereMaybe
      where
        renderFields [] = mempty
        renderFields fs = commas $ map (\(field, val) -> fromMySQLFieldName field <> emit " = " <> fromMySQLExpression val) fs
