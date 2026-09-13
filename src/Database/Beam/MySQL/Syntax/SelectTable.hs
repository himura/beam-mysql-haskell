module Database.Beam.MySQL.Syntax.SelectTable
    ( MySQLSelectTableSyntax (..)
    , MySQLSelectSyntax (..)
    , MySQLProjectionSyntax (..)
    , MySQLFromSyntax (..)
    , MySQLTableSourceSyntax (..)
    , MySQLGroupingSyntax (..)
    , MySQLSelectSetQuantifierSyntax (..)
    , MySQLOrderingSyntax (..)
    , MySQLSelectLockingStrength (..)
    , MySQLSelectLockingOption (..)
    , MySQLSelectLockingSyntax (..)
    , mysqlSelectStmt
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.Coerce (coerce)
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Expression (MySQLExpressionSyntax (..))
import Database.Beam.MySQL.Syntax.TableName
import Database.Beam.MySQL.Syntax.Type

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
    :: Maybe MySQLSelectLockingSyntax
    -> MySQLSelectTableSyntax
    -> [MySQLOrderingSyntax]
    -> Maybe Integer
    -> Maybe Integer
    -> MySQLSelectSyntax
mysqlSelectStmt locking tbl ordering limit offset =
    MySQLSelectSyntax $
        fromMySQLSelectTable tbl
            <> ssOrdering
            <> intClause " LIMIT " limit
            <> intClause " OFFSET " offset
            <> maybe mempty fromMySQLSelectLockingSyntax locking
  where
    intClause prefix = maybe mempty (emitBuilder . (Builder.byteString prefix <>) . Builder.string8 . show)

    ssOrdering = case ordering of
        [] -> mempty
        _ -> emit " ORDER BY " <> commas (map fromMySQLOrdering ordering)

newtype MySQLProjectionSyntax = MySQLProjectionSyntax {fromMySQLProjection :: MySQLSyntax}

instance IsSql92ProjectionSyntax MySQLProjectionSyntax where
    type Sql92ProjectionExpressionSyntax MySQLProjectionSyntax = MySQLExpressionSyntax
    projExprs =
        MySQLProjectionSyntax
            . commas
            . map (\(expr, nm) -> fromMySQLExpression expr <> maybe mempty (\n -> emit " AS " <> quotedIdentifier n) nm)

newtype MySQLFromSyntax = MySQLFromSyntax {fromMySQLFrom :: MySQLSyntax}

instance IsSql92FromSyntax MySQLFromSyntax where
    type Sql92FromTableSourceSyntax MySQLFromSyntax = MySQLTableSourceSyntax
    type Sql92FromExpressionSyntax MySQLFromSyntax = MySQLExpressionSyntax
    fromTable tableSrc Nothing = coerce tableSrc
    fromTable tableSrc (Just (name, colNames)) =
        MySQLFromSyntax $
            fromMySQLTableSource tableSrc
                <> emit " AS "
                <> quotedIdentifier name
                <> maybe mempty (parens . commas . map quotedIdentifier) colNames

    innerJoin = mysqlJoin -- In MySQL, JOIN, CROSS JOIN, and INNER JOIN are syntactic equivalents. see 13.2.13.2 JOIN Clause
    leftJoin = mysqlLRJoin "LEFT JOIN"
    rightJoin = mysqlLRJoin "RIGHT JOIN"

mysqlJoin'
    :: ByteString
    -> MySQLFromSyntax
    -> MySQLFromSyntax
    -> MySQLSyntax
mysqlJoin' joinType a b = fromMySQLFrom a <> spaces (emit joinType) <> fromMySQLFrom b

mysqlJoin
    :: MySQLFromSyntax
    -> MySQLFromSyntax
    -> Maybe MySQLExpressionSyntax
    -> MySQLFromSyntax
mysqlJoin a b onCondMay =
    MySQLFromSyntax $ mysqlJoin' "JOIN" a b <> maybe mempty ((emit " ON " <>) . fromMySQLExpression) onCondMay

mysqlLRJoin
    :: ByteString
    -- ^ join type
    -> MySQLFromSyntax
    -> MySQLFromSyntax
    -> Maybe MySQLExpressionSyntax
    -> MySQLFromSyntax
mysqlLRJoin joinType a b onCondMay =
    MySQLFromSyntax $ mysqlJoin' joinType a b <> emit " ON " <> maybe (emit "TRUE") fromMySQLExpression onCondMay

newtype MySQLTableSourceSyntax = MySQLTableSourceSyntax {fromMySQLTableSource :: MySQLSyntax}

instance IsSql92TableSourceSyntax MySQLTableSourceSyntax where
    type Sql92TableSourceSelectSyntax MySQLTableSourceSyntax = MySQLSelectSyntax
    type Sql92TableSourceExpressionSyntax MySQLTableSourceSyntax = MySQLExpressionSyntax
    type Sql92TableSourceTableNameSyntax MySQLTableSourceSyntax = MySQLTableNameSyntax

    tableNamed = MySQLTableSourceSyntax . fromMySQLTableName
    tableFromSubSelect = MySQLTableSourceSyntax . parens . fromMySQLSelect
    tableFromValues vss =
        MySQLTableSourceSyntax
            . parens
            $ emit "VALUES "
                <> commas (map (parens . commas . map fromMySQLExpression) vss)

newtype MySQLGroupingSyntax = MySQLGroupingSyntax {fromMySQLGrouping :: MySQLSyntax}

instance IsSql92GroupingSyntax MySQLGroupingSyntax where
    type Sql92GroupingExpressionSyntax MySQLGroupingSyntax = MySQLExpressionSyntax
    groupByExpressions = MySQLGroupingSyntax . commas . map fromMySQLExpression

newtype MySQLSelectSetQuantifierSyntax = MySQLSelectSetQuantifierSyntax {fromMySQLSelectSetQuantifier :: MySQLSyntax}

instance IsSql92AggregationSetQuantifierSyntax MySQLSelectSetQuantifierSyntax where
    setQuantifierDistinct = MySQLSelectSetQuantifierSyntax $ emit "DISTINCT"
    setQuantifierAll = MySQLSelectSetQuantifierSyntax $ emit "ALL"

newtype MySQLOrderingSyntax = MySQLOrderingSyntax {fromMySQLOrdering :: MySQLSyntax}

instance IsSql92OrderingSyntax MySQLOrderingSyntax where
    type Sql92OrderingExpressionSyntax MySQLOrderingSyntax = MySQLExpressionSyntax
    ascOrdering e = MySQLOrderingSyntax $ fromMySQLExpression e <> emit " ASC"
    descOrdering e = MySQLOrderingSyntax $ fromMySQLExpression e <> emit " DESC"

-- | Lock strength of a locking read.
--
-- Each constructor is rendered verbatim. Which ones are accepted depends on
-- the server; see "Database.Beam.MySQL.Full".
data MySQLSelectLockingStrength
    = -- | @FOR SHARE@ (MySQL 8.0.1+ only; MariaDB does not support it)
      MySQLSelectLockingStrengthShare
    | -- | @FOR UPDATE@
      MySQLSelectLockingStrengthUpdate
    | -- | @LOCK IN SHARE MODE@
      MySQLSelectLockingStrengthLockInShareMode
    deriving stock (Eq, Show)

-- | What a locking read does when a row is locked by another transaction.
--
-- Each constructor is rendered verbatim. Which ones are accepted depends on
-- the server; see "Database.Beam.MySQL.Full".
data MySQLSelectLockingOption
    = -- | @NOWAIT@: fail immediately
      MySQLSelectLockingOptionNoWait
    | -- | @SKIP LOCKED@: exclude locked rows from the result
      MySQLSelectLockingOptionSkipLocked
    | -- | @WAIT n@: wait at most @n@ seconds (MariaDB only)
      MySQLSelectLockingOptionWait Word
    deriving stock (Eq, Show)

data MySQLSelectLockingSyntax = MySQLSelectLockingSyntax
    { lockingStrength :: MySQLSelectLockingStrength
    , lockingOption :: Maybe MySQLSelectLockingOption
    }
    deriving stock (Eq, Show)

fromMySQLSelectLockingSyntax :: MySQLSelectLockingSyntax -> MySQLSyntax
fromMySQLSelectLockingSyntax MySQLSelectLockingSyntax{..} =
    strength lockingStrength <> maybe mempty option lockingOption
  where
    strength = \case
        MySQLSelectLockingStrengthShare -> emit " FOR SHARE"
        MySQLSelectLockingStrengthUpdate -> emit " FOR UPDATE"
        MySQLSelectLockingStrengthLockInShareMode -> emit " LOCK IN SHARE MODE"

    option = \case
        MySQLSelectLockingOptionNoWait -> emit " NOWAIT"
        MySQLSelectLockingOptionSkipLocked -> emit " SKIP LOCKED"
        MySQLSelectLockingOptionWait n -> emitBuilder $ Builder.byteString " WAIT " <> Builder.wordDec n
