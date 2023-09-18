module Database.Beam.MySQL.Syntax.Type
    ( MySQLSyntax (..)
    , emit
    , emitBuilder
    , emitValue

      -- * utils
    , parens
    , spaces
    , sepBy
    , commas
    , quotedIdentifier
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as L
import Data.DList qualified as DL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
emit = emitBuilder . Builder.byteString

emitBuilder :: Builder -> MySQLSyntax
emitBuilder builder = MySQLSyntax (SqlBuilder $ const builder) mempty

emitValue :: MySQLValue -> MySQLSyntax
emitValue v = MySQLSyntax (SqlBuilder ($ v)) (DL.singleton v)

parens :: MySQLSyntax -> MySQLSyntax
parens a = emit "(" <> a <> emit ")"

spaces :: MySQLSyntax -> MySQLSyntax
spaces a = emit " " <> a <> emit " "

sepBy :: MySQLSyntax -> [MySQLSyntax] -> MySQLSyntax
sepBy _ [] = mempty
sepBy _ [x] = x
sepBy sep (x : xs) = x <> foldMap (sep <>) xs

commas :: [MySQLSyntax] -> MySQLSyntax
commas = sepBy (emit ",")

quotedIdentifier :: T.Text -> MySQLSyntax
quotedIdentifier name = emit $ T.encodeUtf8 $ "`" <> quoted <> "`"
  where
    quoted = T.replace "`" "``" name
