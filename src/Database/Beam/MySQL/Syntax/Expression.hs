module Database.Beam.MySQL.Syntax.Expression
    ( MySQLExpressionSyntax (..)
    , MySQLAggregationSetQuantifierSyntax (..)
    , MySQLQuantifierSyntax (..)
    , MySQLFieldNameSyntax (..)
    , MySQLDataTypeSyntax (..)
    , MySQLExtractFieldSyntax (..)
    )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder as Builder
import Data.Coerce
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Database.Beam.Backend.SQL
import {-# SOURCE #-} Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.MySQL.Syntax.Type
import Database.Beam.MySQL.Syntax.Value

newtype MySQLExpressionSyntax = MySQLExpressionSyntax {fromMySQLExpression :: MySQLSyntax} deriving (Eq)

instance IsSql92ExpressionSyntax MySQLExpressionSyntax where
    type Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax = MySQLQuantifierSyntax
    type Sql92ExpressionValueSyntax MySQLExpressionSyntax = MySQLValueSyntax
    type Sql92ExpressionSelectSyntax MySQLExpressionSyntax = MySQLSelectSyntax
    type Sql92ExpressionFieldNameSyntax MySQLExpressionSyntax = MySQLFieldNameSyntax
    type Sql92ExpressionCastTargetSyntax MySQLExpressionSyntax = MySQLDataTypeSyntax
    type Sql92ExpressionExtractFieldSyntax MySQLExpressionSyntax = MySQLExtractFieldSyntax

    valueE = coerce
    rowE = MySQLExpressionSyntax . parens . commas . map fromMySQLExpression
    coalesceE = MySQLExpressionSyntax . (emit "COALESCE" <>) . parens . commas . map fromMySQLExpression
    caseE cases else_ =
        MySQLExpressionSyntax $
            emit "CASE"
                <> foldMap (\(cond, res) -> emit " WHEN " <> fromMySQLExpression cond <> emit " THEN " <> fromMySQLExpression res) cases
                <> emit " ELSE "
                <> fromMySQLExpression else_
                <> emit " END"
    fieldE = coerce
    andE = binOp "AND"
    orE = binOp "OR"
    addE = binOp "+"
    subE = binOp "-"
    mulE = binOp "*"
    divE = binOp "/"
    likeE = binOp "LIKE"
    modE = binOp "%"
    overlapsE = binOp "OVERLAPS" -- may not support?
    nullIfE a b = MySQLExpressionSyntax $ emit "NULLIF" <> parens (commas $ map fromMySQLExpression [a, b])
    positionE needle haystack = MySQLExpressionSyntax $ emit "POSITION" <> parens (parens (fromMySQLExpression needle) <> emit " IN " <> parens (fromMySQLExpression haystack))
    eqE = compOp "="
    neqE = compOp "<>"
    ltE = compOp "<"
    gtE = compOp ">"
    leE = compOp "<="
    geE = compOp ">="
    castE expr t = MySQLExpressionSyntax $ emit "CAST" <> parens (parens (fromMySQLExpression expr) <> emit " AS " <> fromMySQLDataType t)
    notE = unOp "NOT"
    negateE = unOp "-"
    isNullE = postFix "IS NULL"
    isNotNullE = postFix "IS NOT NULL"
    isTrueE = postFix "IS TRUE"
    isNotTrueE = postFix "IS NOT TRUE"
    isFalseE = postFix "IS FALSE"
    isNotFalseE = postFix "IS NOT FALSE"
    isUnknownE = postFix "IS UNKNOWN"
    isNotUnknownE = postFix "IS NOT UNKNOWN"
    charLengthE = funExpr "CHAR_LENGTH"
    octetLengthE = funExpr "OCTET_LENGTH" -- synonym of LENGTH
    bitLengthE = funExpr "BIT_LENGTH"
    lowerE = funExpr "LOWER"
    upperE = funExpr "UPPER"
    trimE = funExpr "TRIM"
    absE = funExpr "ABS"
    extractE field from = MySQLExpressionSyntax $ emit "EXTRACT" <> parens (fromMySQLExtractField field <> emit " FROM " <> parens (fromMySQLExpression from))
    existsE select = MySQLExpressionSyntax $ emit "EXISTS " <> parens (fromMySQLSelect select)
    uniqueE select = MySQLExpressionSyntax $ emit "UNIQUE " <> parens (fromMySQLSelect select)
    subqueryE = MySQLExpressionSyntax . parens . fromMySQLSelect
    currentTimestampE = MySQLExpressionSyntax $ emit "CURRENT_TIMESTAMP"
    defaultE = MySQLExpressionSyntax $ emit "DEFAULT"
    inE e es = MySQLExpressionSyntax $ parens (fromMySQLExpression e) <> emit " IN " <> parens (commas (map fromMySQLExpression es))
    inSelectE e select = MySQLExpressionSyntax $ parens (fromMySQLExpression e) <> emit " IN " <> parens (fromMySQLSelect select)

binOp :: ByteString -> MySQLExpressionSyntax -> MySQLExpressionSyntax -> MySQLExpressionSyntax
binOp op a b = MySQLExpressionSyntax $ parens (fromMySQLExpression a) <> spaces (emit op) <> parens (fromMySQLExpression b)

postFix :: ByteString -> MySQLExpressionSyntax -> MySQLExpressionSyntax
postFix op a = MySQLExpressionSyntax $ parens (fromMySQLExpression a) <> emit " " <> emit op

unOp :: ByteString -> MySQLExpressionSyntax -> MySQLExpressionSyntax
unOp op a = MySQLExpressionSyntax $ emit op <> parens (fromMySQLExpression a)

compOp :: ByteString -> Maybe MySQLQuantifierSyntax -> MySQLExpressionSyntax -> MySQLExpressionSyntax -> MySQLExpressionSyntax
compOp op quantifier a b =
    MySQLExpressionSyntax $
        parens (fromMySQLExpression a)
            <> spaces (emit op)
            <> maybe mempty ((<> emit " ") . fromMySQLQuantifier) quantifier
            <> parens (fromMySQLExpression b)

funExpr :: ByteString -> MySQLExpressionSyntax -> MySQLExpressionSyntax
funExpr f a = MySQLExpressionSyntax $ emit f <> parens (fromMySQLExpression a)

instance IsSql92AggregationExpressionSyntax MySQLExpressionSyntax where
    type Sql92AggregationSetQuantifierSyntax MySQLExpressionSyntax = MySQLAggregationSetQuantifierSyntax
    countAllE = MySQLExpressionSyntax $ emit "COUNT(*)"
    countE = unAgg "COUNT"
    avgE = unAgg "AVG"
    maxE = unAgg "MAX"
    minE = unAgg "MIN"
    sumE = unAgg "SUM"

unAgg :: ByteString -> Maybe MySQLAggregationSetQuantifierSyntax -> MySQLExpressionSyntax -> MySQLExpressionSyntax
unAgg fn quantifier expr =
    MySQLExpressionSyntax $
        emit fn
            <> parens (maybe mempty (\q -> fromMySQLAggregationSetQuantifier q <> emit " ") quantifier)
            <> fromMySQLExpression expr

newtype MySQLAggregationSetQuantifierSyntax = MySQLAggregationSetQuantifierSyntax {fromMySQLAggregationSetQuantifier :: MySQLSyntax}

instance IsSql92AggregationSetQuantifierSyntax MySQLAggregationSetQuantifierSyntax where
    setQuantifierDistinct = MySQLAggregationSetQuantifierSyntax $ emit "DISTINCT"
    setQuantifierAll = MySQLAggregationSetQuantifierSyntax $ emit "ALL"

newtype MySQLQuantifierSyntax = MySQLQuantifierSyntax {fromMySQLQuantifier :: MySQLSyntax}

instance IsSql92QuantifierSyntax MySQLQuantifierSyntax where
    quantifyOverAll = MySQLQuantifierSyntax $ emit "ALL"
    quantifyOverAny = MySQLQuantifierSyntax $ emit "ANY"

newtype MySQLFieldNameSyntax = MySQLFieldNameSyntax {fromMySQLFieldName :: MySQLSyntax}

instance IsSql92FieldNameSyntax MySQLFieldNameSyntax where
    qualifiedField tbl col = MySQLFieldNameSyntax $ quotedIdentifier tbl <> emit "." <> quotedIdentifier col
    unqualifiedField col = MySQLFieldNameSyntax $ quotedIdentifier col

-- TODO: support migration
newtype MySQLDataTypeSyntax = MySQLDataTypeSyntax {fromMySQLDataType :: MySQLSyntax}

instance IsSql92DataTypeSyntax MySQLDataTypeSyntax where
    domainType = MySQLDataTypeSyntax . quotedIdentifier
    charType prec charSet = MySQLDataTypeSyntax $ emit "CHAR" <> mysqlOptPrec prec <> mysqlCharSet charSet
    varCharType prec charSet = MySQLDataTypeSyntax $ emit "VARCHAR" <> mysqlOptPrec prec <> mysqlCharSet charSet
    nationalCharType prec = mysqlType "NATIONAL CHAR" prec
    nationalVarCharType prec = mysqlType "NATIONAL VARCHAR" prec
    bitType prec = mysqlType "BIT" prec
    varBitType prec = mysqlType "BIT" prec -- BIT VARYING is not supported
    numericType prec = MySQLDataTypeSyntax $ emit "NUMERIC" <> mysqlOptNumericPrec prec -- same as decimal
    decimalType prec = MySQLDataTypeSyntax $ emit "DECIMAL" <> mysqlOptNumericPrec prec
    intType = mysqlType "INT" Nothing
    smallIntType = mysqlType "SMALLINT" Nothing
    floatType = mysqlType "FLOAT"
    doubleType = mysqlType "DOUBLE" Nothing -- can take precision
    realType = mysqlType "REAL" Nothing -- can take precision
    dateType = mysqlType "DATE" Nothing
    timeType prec _tz = mysqlType "TIME" prec -- mysql TIME is timezone agnostic
    timestampType prec _tz = mysqlType "TIMESTAMP" prec -- mysql TIMESTAMP is timezone agnostic

mysqlType :: ByteString -> Maybe Word -> MySQLDataTypeSyntax
mysqlType tname prec =
    MySQLDataTypeSyntax $ emit tname <> mysqlOptPrec prec

mysqlOptPrec :: Maybe Word -> MySQLSyntax
mysqlOptPrec Nothing = mempty
mysqlOptPrec (Just prec) = parens $ emitBuilder (Builder.string7 (show prec))

mysqlOptNumericPrec :: Maybe (Word, Maybe Word) -> MySQLSyntax
mysqlOptNumericPrec Nothing = mempty
mysqlOptNumericPrec (Just (prec, Nothing)) = mysqlOptPrec $ Just prec
mysqlOptNumericPrec (Just (prec, Just dec)) = parens $ emitBuilder (Builder.string7 (show prec)) <> emit "," <> emitBuilder (Builder.string7 (show dec))

mysqlCharSet :: Maybe Text -> MySQLSyntax
mysqlCharSet Nothing = mempty
mysqlCharSet (Just cs) = emit " CHARACTER SET " <> emit (T.encodeUtf8 cs)

newtype MySQLExtractFieldSyntax = MySQLExtractFieldSyntax {fromMySQLExtractField :: MySQLSyntax}

instance IsSql92ExtractFieldSyntax MySQLExtractFieldSyntax where
    secondsField = MySQLExtractFieldSyntax $ emit "SECOND"
    minutesField = MySQLExtractFieldSyntax $ emit "MINUTE"
    hourField = MySQLExtractFieldSyntax $ emit "HOUR"
    dayField = MySQLExtractFieldSyntax $ emit "DAY"
    monthField = MySQLExtractFieldSyntax $ emit "MONTH"
    yearField = MySQLExtractFieldSyntax $ emit "YEAR"
