module Database.Beam.MySQL.Syntax.Insert
    ( MySQLInsertSyntax (..)
    ) where

import Data.Coerce
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Expression
import Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.MySQL.Syntax.TableName
import Database.Beam.MySQL.Syntax.Type

data MySQLInsertSyntax = MySQLInsertSyntax
    { tableName :: MySQLTableNameSyntax
    , fromMySQLInsert :: MySQLSyntax
    }

instance IsSql92InsertSyntax MySQLInsertSyntax where
    type Sql92InsertValuesSyntax MySQLInsertSyntax = MySQLInsertValuesSyntax
    type Sql92InsertTableNameSyntax MySQLInsertSyntax = MySQLTableNameSyntax
    insertStmt tblName fields values =
        MySQLInsertSyntax
            { tableName = tblName
            , fromMySQLInsert =
                emit "INSERT INTO "
                    <> fromMySQLTableName tblName
                    <> parens (commas (map quotedIdentifier fields))
                    <> emit " "
                    <> fromMySQLInsertValues values
            }

newtype MySQLInsertValuesSyntax = MySQLInsertValuesSyntax {fromMySQLInsertValues :: MySQLSyntax}

instance IsSql92InsertValuesSyntax MySQLInsertValuesSyntax where
    type Sql92InsertValuesExpressionSyntax MySQLInsertValuesSyntax = MySQLExpressionSyntax
    type Sql92InsertValuesSelectSyntax MySQLInsertValuesSyntax = MySQLSelectSyntax
    insertSqlExpressions es =
        MySQLInsertValuesSyntax $
            emit "VALUES "
                <> commas (map (parens . commas . map fromMySQLExpression) es)
    insertFromSql = coerce
