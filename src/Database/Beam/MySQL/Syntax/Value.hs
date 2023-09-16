{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.MySQL.Syntax.Value
    ( MySQLValueSyntax (..)
    ) where

import Data.Int
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Word

import Database.Beam.Backend.Internal.Compat
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Type
import Database.MySQL.Base (MySQLValue (..))
import GHC.TypeLits

newtype MySQLValueSyntax = MySQLValueSyntax {fromMySQLValue :: MySQLSyntax}

defaultValueSyntax :: MySQLValue -> MySQLValueSyntax
defaultValueSyntax = MySQLValueSyntax . emitValue

-- TODO support other types
instance HasSqlValueSyntax MySQLValueSyntax Int64 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt64
instance HasSqlValueSyntax MySQLValueSyntax Int32 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt32
instance HasSqlValueSyntax MySQLValueSyntax Int16 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt16
instance HasSqlValueSyntax MySQLValueSyntax Int8 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt8

instance HasSqlValueSyntax MySQLValueSyntax Word64 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt64U
instance HasSqlValueSyntax MySQLValueSyntax Word32 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt32U
instance HasSqlValueSyntax MySQLValueSyntax Word16 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt16U
instance HasSqlValueSyntax MySQLValueSyntax Word8 where
    sqlValueSyntax = defaultValueSyntax . MySQLInt8U

instance HasSqlValueSyntax MySQLValueSyntax Scientific where
    sqlValueSyntax = defaultValueSyntax . MySQLDecimal
instance HasSqlValueSyntax MySQLValueSyntax Float where
    sqlValueSyntax = defaultValueSyntax . MySQLFloat
instance HasSqlValueSyntax MySQLValueSyntax Double where
    sqlValueSyntax = defaultValueSyntax . MySQLDouble

instance HasSqlValueSyntax MySQLValueSyntax LocalTime where
    sqlValueSyntax = defaultValueSyntax . MySQLTimeStamp
instance HasSqlValueSyntax MySQLValueSyntax Day where
    sqlValueSyntax = defaultValueSyntax . MySQLDate

instance HasSqlValueSyntax MySQLValueSyntax T.Text where
    sqlValueSyntax = defaultValueSyntax . MySQLText
instance HasSqlValueSyntax MySQLValueSyntax TL.Text where
    sqlValueSyntax = sqlValueSyntax . TL.toStrict
instance HasSqlValueSyntax MySQLValueSyntax String where
    sqlValueSyntax = sqlValueSyntax . T.pack

instance HasSqlValueSyntax MySQLValueSyntax Bool where
    sqlValueSyntax = sqlValueSyntax . (\b -> if b then 1 else 0 :: Int8)

instance HasSqlValueSyntax MySQLValueSyntax SqlNull where
    sqlValueSyntax _ = defaultValueSyntax MySQLNull

instance
    (HasSqlValueSyntax MySQLValueSyntax a)
    => HasSqlValueSyntax MySQLValueSyntax (Maybe a)
    where
    sqlValueSyntax Nothing = sqlValueSyntax SqlNull
    sqlValueSyntax (Just a) = sqlValueSyntax a

instance (TypeError (PreferExplicitSize Int Int32)) => HasSqlValueSyntax MySQLValueSyntax Int where
    sqlValueSyntax = error "TypeError"

instance (TypeError (PreferExplicitSize Int Word32)) => HasSqlValueSyntax MySQLValueSyntax Word where
    sqlValueSyntax = error "TypeError"
