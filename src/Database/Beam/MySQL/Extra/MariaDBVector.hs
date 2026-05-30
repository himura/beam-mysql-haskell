{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Support for the @VECTOR@ data type and vector functions introduced in
-- MariaDB 11.7+.
--
-- See the MariaDB documentation:
-- <https://mariadb.com/kb/en/vector-overview/>.
--
-- == Usage
--
-- Declare a column with type 'MariaDBVector' in your beam schema. Construct
-- values with 'fromFloats' and read them back with 'toFloats':
--
-- > insertedVec = fromFloats [0.1, 0.2, 0.3]
--
-- Use 'vecDistance_', 'vecDistanceCosine_', or 'vecDistanceEuclidean_' in
-- @ORDER BY@ clauses to perform nearest-neighbour searches.
--
-- This module is intended to be imported qualified:
--
-- > import qualified Database.Beam.MySQL.Extra.MariaDBVector as MDV
module Database.Beam.MySQL.Extra.MariaDBVector
    ( MariaDBVector (..)

      -- * Conversion between @[Float]@ and 'MariaDBVector'
    , fromFloats
    , toFloats

      -- * SQL functions

      -- ** Distance
    , vecDistance_
    , vecDistanceCosine_
    , vecDistanceEuclidean_

      -- ** Text representation
    , vecFromText_
    , vecToText_
    ) where

import Data.Binary.Get (Get, getFloatle, isEmpty, runGetOrFail)
import Data.Binary.Put (putFloatle, runPut)
import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as L
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

-- | A value of the MariaDB @VECTOR(N)@ type.
--
-- MariaDB stores vectors as a packed sequence of N IEEE 754 single-precision
-- floats in little-endian byte order. This newtype wraps that raw byte
-- representation directly; use 'fromFloats' / 'toFloats' to convert to and
-- from @[Float]@.
--
-- The wrapped 'S.ByteString' length must be a multiple of 4 (one @Float@
-- per 4 bytes). 'fromFloats' guarantees this; if you construct a value
-- through the 'MariaDBVector' constructor directly, you are responsible
-- for maintaining the invariant.
newtype MariaDBVector = MariaDBVector {unMariaDBVector :: S.ByteString}
    deriving stock (Show, Eq, Ord)

-- | Encode a list of single-precision floats into MariaDB's binary vector
-- representation (little-endian @float32@ sequence). Total.
fromFloats :: [Float] -> MariaDBVector
fromFloats = MariaDBVector . L.toStrict . runPut . mapM_ putFloatle

-- | Decode a 'MariaDBVector' back into a list of 'Float'.
--
-- Returns 'Left' if the underlying byte string is not a whole number of
-- 4-byte floats. Values produced by 'fromFloats' or read back from a
-- well-formed @VECTOR@ column always decode successfully; a failure
-- typically indicates a corrupted or hand-constructed 'MariaDBVector'.
toFloats :: MariaDBVector -> Either String [Float]
toFloats (MariaDBVector bs) =
    case runGetOrFail getFloats (L.fromStrict bs) of
        Left (_, _, err) -> Left err
        Right (_, _, xs) -> Right xs
  where
    getFloats :: Get [Float]
    getFloats = do
        e <- isEmpty
        if e then pure [] else (:) <$> getFloatle <*> getFloats

instance FromBackendRow MySQL MariaDBVector

instance FromField MariaDBVector where
    fromField = fmap MariaDBVector . fromField

instance HasSqlValueSyntax MySQLValueSyntax MariaDBVector where
    sqlValueSyntax = sqlValueSyntax . unMariaDBVector

-- | The MariaDB @VEC_DISTANCE(a, b)@ function.
--
-- Computes the distance between two vectors using the metric configured
-- for the column's vector index (set when the index is created). Prefer
-- this over 'vecDistanceCosine_' / 'vecDistanceEuclidean_' when querying
-- through an index, so that the executor can use the index.
--
-- See <https://mariadb.com/kb/en/vec_distance/>.
vecDistance_
    :: QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s Double
vecDistance_ = vecDistance_' "VEC_DISTANCE"

-- | The MariaDB @VEC_DISTANCE_COSINE(a, b)@ function.
--
-- Always computes cosine distance regardless of the column's index metric.
--
-- See <https://mariadb.com/kb/en/vec_distance_cosine/>.
vecDistanceCosine_
    :: QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s Double
vecDistanceCosine_ = vecDistance_' "VEC_DISTANCE_COSINE"

-- | The MariaDB @VEC_DISTANCE_EUCLIDEAN(a, b)@ function.
--
-- Always computes Euclidean (L2) distance regardless of the column's index
-- metric.
--
-- See <https://mariadb.com/kb/en/vec_distance_euclidean/>.
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

-- | The MariaDB @VEC_FromText(s)@ function.
--
-- Parses a JSON array literal such as @"[0.1, 0.2, 0.3]"@ into the binary
-- vector representation. Useful for inserting vectors from textual sources
-- or in @WHERE@/@ORDER BY@ clauses without round-tripping through Haskell.
--
-- See <https://mariadb.com/kb/en/vec_fromtext/>.
vecFromText_
    :: (BeamSqlBackendIsString MySQL text)
    => QGenExpr ctxt MySQL s text
    -> QGenExpr ctxt MySQL s MariaDBVector
vecFromText_ (QExpr vect) =
    QExpr $ \t -> MySQLExpressionSyntax $ emit "VEC_FromText" <> (parens (fromMySQLExpression (vect t)))

-- | The MariaDB @VEC_ToText(v)@ function.
--
-- Renders a vector as a JSON array literal, e.g. @"[0.1,0.2,0.3]"@. Useful
-- for debugging or for exporting vectors without decoding them on the
-- client side.
--
-- See <https://mariadb.com/kb/en/vec_totext/>.
vecToText_
    :: (BeamSqlBackendIsString MySQL text)
    => QGenExpr ctxt MySQL s MariaDBVector
    -> QGenExpr ctxt MySQL s text
vecToText_ (QExpr vtxt) =
    QExpr $ \t -> MySQLExpressionSyntax $ emit "VEC_ToText" <> (parens (fromMySQLExpression (vtxt t)))
