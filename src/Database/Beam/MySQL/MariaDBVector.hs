{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database.Beam.MySQL.MariaDBVector
    ( MariaDBVector (..)
    , vecDistance_
    , vecDistanceCosine_
    , vecDistanceEuclidean_
    , vecFromText_
    , vecToText_
    , floatsToMariaDBVector
    ) where

import Data.ByteString qualified as S
import Data.Serialize (putFloat32le, runPut)
import Database.Beam.Backend
import Database.Beam.MySQL.Backend (MySQL)
import Database.Beam.MySQL.FromField
import Database.Beam.MySQL.Syntax.Expression
    ( MySQLExpressionSyntax
        ( MySQLExpressionSyntax
        , fromMySQLExpression
        )
    )
import Database.Beam.MySQL.Syntax.Type
import Database.Beam.MySQL.Syntax.Value
import Database.Beam.Query (QGenExpr (..))

newtype MariaDBVector = MariaDBVector {unMariaDBVector :: S.ByteString}
    deriving stock (Show, Eq, Ord)

floatsToMariaDBVector :: [Float] -> MariaDBVector
floatsToMariaDBVector = MariaDBVector . runPut . mapM_ putFloat32le

-- TODO: toFloats
-- toFloats :: MariaDBVector -> [Float]
-- toFloats = runGet . mapM_

instance FromBackendRow MySQL MariaDBVector

instance FromField MariaDBVector where
    fromField = fmap MariaDBVector . fromField

instance HasSqlValueSyntax MySQLValueSyntax MariaDBVector where
    sqlValueSyntax = sqlValueSyntax . unMariaDBVector

vecDistance_
    :: QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s Double
vecDistance_ = vecDistance_' "VEC_DISTANCE"

vecDistanceCosine_
    :: QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s Double
vecDistanceCosine_ = vecDistance_' "VEC_DISTANCE_COSINE"

vecDistanceEuclidean_
    :: QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s Double
vecDistanceEuclidean_ = vecDistance_' "VEC_DISTANCE_EUCLIDEAN"

vecDistance_'
    :: S.ByteString
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s Double
vecDistance_' fn (QExpr a) (QExpr b) =
    QExpr $ \t ->
        MySQLExpressionSyntax $
            emit fn <> (parens . commas . map fromMySQLExpression $ [a t, b t])

vecFromText_
    :: (BeamSqlBackendIsString MySQL text)
    => QGenExpr ctxt MySQL s text
    -> QGenExpr ctxt MySQL s MariaDBVector
vecFromText_ (QExpr vect) =
    QExpr $ \t -> MySQLExpressionSyntax $ emit "VEC_FromText" <> (parens (fromMySQLExpression (vect t)))

vecToText_
    :: (BeamSqlBackendIsString MySQL text)
    => QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s text
vecToText_ (QExpr vtxt) =
    QExpr $ \t -> MySQLExpressionSyntax $ emit "VEC_ToText" <> (parens (fromMySQLExpression (vtxt t)))
