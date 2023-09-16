module Database.Beam.MySQL.Syntax.Type
    ( MySQLSyntax (..)
    , emit
    , emitBuilder
    , emitValue

      -- * utils
    , parens
    , spaces
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder as Builder
    ( Builder
    , byteString
    , toLazyByteString
    )
import Data.ByteString.Lazy qualified as L
import Data.DList qualified as DL
import Database.MySQL.Base (MySQLValue)

data MySQLSyntax = MySQLSyntax
    { buildSql :: SqlBuilder
    , params :: DL.DList MySQLValue
    }
    deriving (Eq, Show)

instance Semigroup MySQLSyntax where
    MySQLSyntax ab ap <> MySQLSyntax bb bp =
        MySQLSyntax (ab <> bb) (ap <> bp)

instance Monoid MySQLSyntax where
    mempty = MySQLSyntax mempty mempty

newtype SqlBuilder = SqlBuilder ((MySQLValue -> Builder) -> Builder)

instance Show SqlBuilder where
    show = show . buildSqlWithPlaceholder

instance Semigroup SqlBuilder where
    SqlBuilder a <> SqlBuilder b = SqlBuilder (\v -> a v <> b v)

instance Monoid SqlBuilder where
    mempty = SqlBuilder (const mempty)

instance Eq SqlBuilder where
    a == b = buildSqlWithPlaceholder a == buildSqlWithPlaceholder b

buildSqlWithPlaceholder :: SqlBuilder -> L.ByteString
buildSqlWithPlaceholder (SqlBuilder builder) = Builder.toLazyByteString $ builder (const "?")

emit :: ByteString -> MySQLSyntax
emit = emitBuilder . byteString

emitBuilder :: Builder -> MySQLSyntax
emitBuilder builder = MySQLSyntax (SqlBuilder $ const builder) mempty

emitValue :: MySQLValue -> MySQLSyntax
emitValue v = MySQLSyntax (SqlBuilder ($ v)) (DL.singleton v)

parens :: MySQLSyntax -> MySQLSyntax
parens a = emit "(" <> a <> emit ")"

spaces :: MySQLSyntax -> MySQLSyntax
spaces a = emit " " <> a <> emit " "
