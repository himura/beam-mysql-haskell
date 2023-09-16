module Database.Beam.MySQL.Syntax.SelectTable where

import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.ByteString.Builder as Builder
import Data.ByteString.Lazy qualified as L
import Data.DList qualified as DL
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Type
import Database.MySQL.Base

newtype MySQLSelectTableSyntax = MySQLSelectTableSyntax {fromMySQLSelectTable :: MySQLSyntax} deriving (Eq, Show)

instance IsSql92SelectTableSyntax MySQLSelectTableSyntax where
    type Sql92SelectTableSelectSyntax MySQLSelectTableSyntax = MySQLSelectSyntax
    type Sql92SelectTableExpressionSyntax MySQLSelectTableSyntax = MySQLExpressionSyntax
    type Sql92SelectTableProjectionSyntax MySQLSelectTableSyntax = MySQLProjectionSyntax
    type Sql92SelectTableFromSyntax MySQLSelectTableSyntax = MySQLFromSyntax
    type Sql92SelectTableGroupingSyntax MySQLSelectTableSyntax = MySQLGroupingSyntax
    type Sql92SelectTableSetQuantifierSyntax MySQLSelectTableSyntax = MySQLSelectSetQuantifierSyntax

    selectTableStmt setQuantifier proj from where_ grouping having =
        MySQLSelectTableSyntax $
            emit "SELECT "
                <> maybe mempty (\setQuantifier' -> fromMySQLSelectSetQuantifier setQuantifier' <> emit " ") setQuantifier
                <> fromMySQLProjection proj
                <> maybe mempty ((emit " FROM " <>) . fromMySQLFrom) from
                <> maybe mempty ((emit " WHERE " <>) . fromMySQLExpression) where_
                <> maybe mempty ((emit " GROUP BY " <>) . fromMySQLGrouping) grouping
                <> maybe mempty ((emit " HAVING " <>) . fromMySQLExpression) having

    unionTables isAll = tableOp (if isAll then "UNION ALL" else "UNION")
    intersectTables isAll = tableOp (if isAll then "INTERSECT ALL" else "INTERSECT")
    exceptTable isAll = tableOp (if isAll then "EXCEPT ALL" else "EXCEPT")

tableOp
    :: ByteString
    -> MySQLSelectTableSyntax
    -> MySQLSelectTableSyntax
    -> MySQLSelectTableSyntax
tableOp op tbl1 tbl2 =
    MySQLSelectTableSyntax $
        parens (fromMySQLSelectTable tbl1)
            <> spaces (emit op)
            <> parens (fromMySQLSelectTable tbl2)

-- | MySQL @SELECT@ Syntax
newtype MySQLSelectSyntax = MySQLSelectSyntax {fromMySQLSelect :: MySQLSyntax} deriving (Eq, Show)

instance IsSql92SelectSyntax MySQLSelectSyntax where
    type Sql92SelectSelectTableSyntax MySQLSelectSyntax = MySQLSelectTableSyntax
    type Sql92SelectOrderingSyntax MySQLSelectSyntax = MySQLOrderingSyntax
    selectStmt = mysqlSelectStmt Nothing

mysqlSelectStmt
    :: Maybe MySQLSelectForUpdateSyntax
    -> MySQLSelectTableSyntax
    -> [MySQLOrderingSyntax]
    -> Maybe Integer
    -> Maybe Integer
    -> MySQLSelectSyntax
mysqlSelectStmt forUpdate tbl ordering limit offset =
    MySQLSelectSyntax $
        fromMySQLSelectTable tbl
            <> ssOrdering
            <> intClause " LIMIT " limit
            <> intClause " OFFSET " offset
            <> maybe mempty fromMySQLSelectForUpdate forUpdate
  where
    intClause prefix = maybe mempty (emitBuilder . (byteString prefix <>) . Builder.string8 . show)

    ssOrdering = case ordering of
        [] -> mempty
        _ -> emit " ORDER BY " <> commas (map fromMySQLOrdering ordering)

newtype MySQLProjectionSyntax = MySQLProjectionSyntax {fromMySQLProjection :: MySQLSyntax}
newtype MySQLFromSyntax = MySQLFromSyntax {fromMySQLFrom :: MySQLSyntax}
newtype MySQLGroupingSyntax = MySQLGroupingSyntax {fromMySQLGrouping :: MySQLSyntax}
newtype MySQLSelectSetQuantifierSyntax = MySQLSelectSetQuantifierSyntax {fromMySQLSelectSetQuantifier :: MySQLSyntax}
newtype MySQLOrderingSyntax = MySQLOrderingSyntax {fromMySQLOrdering :: MySQLSyntax}

-- TODO
data MySQLSelectForUpdateSyntax = MySQLSelectForUpdateSyntax

fromMySQLSelectForUpdate :: MySQLSelectForUpdateSyntax -> MySQLSyntax
fromMySQLSelectForUpdate = error "not implemented"

newtype MySQLExpressionSyntax = MySQLExpressionSyntax {fromMySQLExpression :: MySQLSyntax} deriving (Eq)
