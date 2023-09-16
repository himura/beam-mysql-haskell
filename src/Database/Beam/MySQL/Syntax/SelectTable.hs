module Database.Beam.MySQL.Syntax.SelectTable
    ( MySQLSelectTableSyntax (..)
    , MySQLSelectSyntax (..)
    , MySQLProjectionSyntax (..)
    , MySQLFromSyntax (..)
    , MySQLTableSourceSyntax (..)
    , MySQLTableNameSyntax (..)
    , MySQLGroupingSyntax (..)
    , MySQLSelectSetQuantifierSyntax (..)
    , MySQLOrderingSyntax (..)
    , MySQLSelectForUpdateSyntax (..)
    , MySQLExpressionSyntax (..)
    , MySQLQuantifierSyntax (..)
    , MySQLFieldNameSyntax (..)
    , MySQLDataTypeSyntax (..)
    , MySQLExtractFieldSyntax (..)
    , MySQLAggregationSetQuantifierSyntax (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder as Builder
import Data.Coerce
import Data.Text (Text)
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Type
import Database.Beam.MySQL.Syntax.Value

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
    leftJoin = mysqlLRJoin "LEFT"
    rightJoin = mysqlLRJoin "RIGHT"

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

newtype MySQLTableNameSyntax = MySQLTableNameSyntax {fromMySQLTableName :: MySQLSyntax}

instance IsSql92TableNameSyntax MySQLTableNameSyntax where
    tableName Nothing t = MySQLTableNameSyntax $ quotedIdentifier t
    tableName (Just db) t = MySQLTableNameSyntax $ quotedIdentifier db <> emit "." <> quotedIdentifier t

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

-- TODO
data MySQLSelectForUpdateSyntax = MySQLSelectForUpdateSyntax

fromMySQLSelectForUpdate :: MySQLSelectForUpdateSyntax -> MySQLSyntax
fromMySQLSelectForUpdate = error "not implemented"

newtype MySQLExpressionSyntax = MySQLExpressionSyntax {fromMySQLExpression :: MySQLSyntax} deriving (Eq)

instance IsSql92ExpressionSyntax MySQLExpressionSyntax where
    type Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax = MySQLQuantifierSyntax
    type Sql92ExpressionValueSyntax MySQLExpressionSyntax = MySQLValueSyntax
    type Sql92ExpressionSelectSyntax MySQLExpressionSyntax = MySQLSelectSyntax
    type Sql92ExpressionFieldNameSyntax MySQLExpressionSyntax = MySQLFieldNameSyntax
    type Sql92ExpressionCastTargetSyntax MySQLExpressionSyntax = MySQLDataTypeSyntax
    type Sql92ExpressionExtractFieldSyntax MySQLExpressionSyntax = MySQLExtractFieldSyntax

    valueE
        :: Sql92ExpressionValueSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    valueE = error "Not Implemented"
    rowE :: [MySQLExpressionSyntax] -> MySQLExpressionSyntax
    rowE = error "Not Implemented"
    coalesceE :: [MySQLExpressionSyntax] -> MySQLExpressionSyntax
    coalesceE = error "Not Implemented"
    caseE
        :: [(MySQLExpressionSyntax, MySQLExpressionSyntax)]
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    caseE = error "Not Implemented"
    fieldE
        :: Sql92ExpressionFieldNameSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    fieldE = error "Not Implemented"
    andE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    andE = error "Not Implemented"
    orE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    orE = error "Not Implemented"
    addE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    addE = error "Not Implemented"
    subE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    subE = error "Not Implemented"
    mulE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    mulE = error "Not Implemented"
    divE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    divE = error "Not Implemented"
    likeE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    likeE = error "Not Implemented"
    modE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    modE = error "Not Implemented"
    overlapsE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    overlapsE = error "Not Implemented"
    nullIfE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    nullIfE = error "Not Implemented"
    positionE
        :: MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    positionE = error "Not Implemented"
    eqE
        :: Maybe (Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax)
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    eqE = error "Not Implemented"
    neqE
        :: Maybe (Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax)
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    neqE = error "Not Implemented"
    ltE
        :: Maybe (Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax)
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    ltE = error "Not Implemented"
    gtE
        :: Maybe (Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax)
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    gtE = error "Not Implemented"
    leE
        :: Maybe (Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax)
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    leE = error "Not Implemented"
    geE
        :: Maybe (Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax)
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    geE = error "Not Implemented"
    castE
        :: MySQLExpressionSyntax
        -> Sql92ExpressionCastTargetSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    castE = error "Not Implemented"
    notE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    notE = error "Not Implemented"
    negateE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    negateE = error "Not Implemented"
    isNullE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isNullE = error "Not Implemented"
    isNotNullE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isNotNullE = error "Not Implemented"
    isTrueE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isTrueE = error "Not Implemented"
    isNotTrueE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isNotTrueE = error "Not Implemented"
    isFalseE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isFalseE = error "Not Implemented"
    isNotFalseE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isNotFalseE = error "Not Implemented"
    isUnknownE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isUnknownE = error "Not Implemented"
    isNotUnknownE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    isNotUnknownE = error "Not Implemented"
    charLengthE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    charLengthE = error "Not Implemented"
    octetLengthE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    octetLengthE = error "Not Implemented"
    bitLengthE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    bitLengthE = error "Not Implemented"
    lowerE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    lowerE = error "Not Implemented"
    upperE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    upperE = error "Not Implemented"
    trimE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    trimE = error "Not Implemented"
    absE :: MySQLExpressionSyntax -> MySQLExpressionSyntax
    absE = error "Not Implemented"
    extractE
        :: Sql92ExpressionExtractFieldSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    extractE = error "Not Implemented"
    existsE
        :: Sql92ExpressionSelectSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    existsE = error "Not Implemented"
    uniqueE
        :: Sql92ExpressionSelectSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    uniqueE = error "Not Implemented"
    subqueryE
        :: Sql92ExpressionSelectSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    subqueryE = error "Not Implemented"
    currentTimestampE :: MySQLExpressionSyntax
    currentTimestampE = error "Not Implemented"
    defaultE :: MySQLExpressionSyntax
    defaultE = error "Not Implemented"
    inE
        :: MySQLExpressionSyntax
        -> [MySQLExpressionSyntax]
        -> MySQLExpressionSyntax
    inE = error "Not Implemented"
    inSelectE
        :: MySQLExpressionSyntax
        -> Sql92ExpressionSelectSyntax MySQLExpressionSyntax
        -> MySQLExpressionSyntax
    inSelectE = error "Not Implemented"

instance IsSql92AggregationExpressionSyntax MySQLExpressionSyntax where
    type Sql92AggregationSetQuantifierSyntax MySQLExpressionSyntax = MySQLAggregationSetQuantifierSyntax
    countAllE = error "Not Implemented"
    countE = error "Not Implemented"
    avgE = error "Not Implemented"
    maxE = error "Not Implemented"
    minE = error "Not Implemented"
    sumE = error "Not Implemented"

newtype MySQLQuantifierSyntax = MySQLQuantifierSyntax {fromMySQLQuantifier :: MySQLSyntax}

instance IsSql92QuantifierSyntax MySQLQuantifierSyntax where
    quantifyOverAll = MySQLQuantifierSyntax $ emit "ALL"
    quantifyOverAny = MySQLQuantifierSyntax $ emit "ANY"

newtype MySQLFieldNameSyntax = MySQLFieldNameSyntax {fromMySQLFieldName :: MySQLSyntax}

instance IsSql92FieldNameSyntax MySQLFieldNameSyntax where
    qualifiedField tbl col = MySQLFieldNameSyntax $ quotedIdentifier tbl <> emit "." <> quotedIdentifier col
    unqualifiedField col = MySQLFieldNameSyntax $ quotedIdentifier col

newtype MySQLDataTypeSyntax = MySQLDataTypeSyntax {fromMySQLDataType :: MySQLSyntax}

instance IsSql92DataTypeSyntax MySQLDataTypeSyntax where
    domainType :: Text -> MySQLDataTypeSyntax
    domainType = error "Not Implemented"
    charType :: Maybe Word -> Maybe Text -> MySQLDataTypeSyntax
    charType = error "Not Implemented"
    varCharType :: Maybe Word -> Maybe Text -> MySQLDataTypeSyntax
    varCharType = error "Not Implemented"
    nationalCharType :: Maybe Word -> MySQLDataTypeSyntax
    nationalCharType = error "Not Implemented"
    nationalVarCharType :: Maybe Word -> MySQLDataTypeSyntax
    nationalVarCharType = error "Not Implemented"
    bitType :: Maybe Word -> MySQLDataTypeSyntax
    bitType = error "Not Implemented"
    varBitType :: Maybe Word -> MySQLDataTypeSyntax
    varBitType = error "Not Implemented"
    numericType :: Maybe (Word, Maybe Word) -> MySQLDataTypeSyntax
    numericType = error "Not Implemented"
    decimalType :: Maybe (Word, Maybe Word) -> MySQLDataTypeSyntax
    decimalType = error "Not Implemented"
    intType :: MySQLDataTypeSyntax
    intType = error "Not Implemented"
    smallIntType :: MySQLDataTypeSyntax
    smallIntType = error "Not Implemented"
    floatType :: Maybe Word -> MySQLDataTypeSyntax
    floatType = error "Not Implemented"
    doubleType :: MySQLDataTypeSyntax
    doubleType = error "Not Implemented"
    realType :: MySQLDataTypeSyntax
    realType = error "Not Implemented"
    dateType :: MySQLDataTypeSyntax
    dateType = error "Not Implemented"
    timeType :: Maybe Word -> Bool -> MySQLDataTypeSyntax
    timeType = error "Not Implemented"
    timestampType :: Maybe Word -> Bool -> MySQLDataTypeSyntax
    timestampType = error "Not Implemented"

newtype MySQLExtractFieldSyntax = MySQLExtractFieldSyntax {fromMySQLExtractField :: MySQLSyntax}

instance IsSql92ExtractFieldSyntax MySQLExtractFieldSyntax where
    secondsField :: MySQLExtractFieldSyntax
    secondsField = error "Not Implemented"
    minutesField :: MySQLExtractFieldSyntax
    minutesField = error "Not Implemented"
    hourField :: MySQLExtractFieldSyntax
    hourField = error "Not Implemented"
    dayField :: MySQLExtractFieldSyntax
    dayField = error "Not Implemented"
    monthField :: MySQLExtractFieldSyntax
    monthField = error "Not Implemented"
    yearField :: MySQLExtractFieldSyntax
    yearField = error "Not Implemented"

newtype MySQLAggregationSetQuantifierSyntax = MySQLAggregationSetQuantifierSyntax {fromMySQLAggregationSetQuantifier :: MySQLSyntax}

instance IsSql92AggregationSetQuantifierSyntax MySQLAggregationSetQuantifierSyntax where
    setQuantifierDistinct = MySQLAggregationSetQuantifierSyntax $ emit "DISTINCT"
    setQuantifierAll = MySQLAggregationSetQuantifierSyntax $ emit "ALL"
