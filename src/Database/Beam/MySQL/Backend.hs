{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.MySQL.Backend
    ( MySQL (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Time (Day, LocalTime, TimeOfDay)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam
    ( FromBackendRow (..)
    , HasQBuilder (..)
    , HasSqlEqualityCheck
    , HasSqlQuantifiedEqualityCheck
    )
import Database.Beam.Backend
    ( BeamBackend (..)
    , BeamSqlBackend
    , BeamSqlBackendIsString
    , BeamSqlBackendSyntax
    , SqlNull
    )
import Database.Beam.Backend.Internal.Compat (PreferExplicitSize)
import Database.Beam.MySQL.FromField (FromField)
import Database.Beam.MySQL.Syntax (MySQLCommandSyntax)
import Database.Beam.Query (HasSqlInTable)
import Database.Beam.Query.SQL92 (buildSql92Query')
import GHC.TypeLits (TypeError)

data MySQL = MySQL

instance BeamSqlBackend MySQL

instance BeamBackend MySQL where
    type BackendFromField MySQL = FromField

type instance BeamSqlBackendSyntax MySQL = MySQLCommandSyntax

instance HasQBuilder MySQL where
    buildSqlQuery = buildSql92Query' True

instance HasSqlInTable MySQL

instance FromBackendRow MySQL SqlNull
instance FromBackendRow MySQL Bool
instance FromBackendRow MySQL Scientific
instance FromBackendRow MySQL Int8
instance FromBackendRow MySQL Int16
instance FromBackendRow MySQL Int32
instance FromBackendRow MySQL Int64

instance (TypeError (PreferExplicitSize Int Int32)) => FromBackendRow MySQL Int where
    fromBackendRow = error "unreachable"

instance FromBackendRow MySQL Word8
instance FromBackendRow MySQL Word16
instance FromBackendRow MySQL Word32
instance FromBackendRow MySQL Word64

instance (TypeError (PreferExplicitSize Word Word32)) => FromBackendRow MySQL Word where
    fromBackendRow = error "unreachable"

instance FromBackendRow MySQL Float
instance FromBackendRow MySQL Double

instance FromBackendRow MySQL ByteString
instance FromBackendRow MySQL L.ByteString
instance FromBackendRow MySQL Text
instance FromBackendRow MySQL TL.Text
instance FromBackendRow MySQL String
instance FromBackendRow MySQL LocalTime
instance FromBackendRow MySQL Day
instance FromBackendRow MySQL TimeOfDay

instance BeamSqlBackendIsString MySQL String
instance BeamSqlBackendIsString MySQL Text

#define MYSQL_HAS_EQUALITY_CHECK(ty)                 \
  instance HasSqlEqualityCheck MySQL (ty);           \
  instance HasSqlQuantifiedEqualityCheck MySQL (ty);

MYSQL_HAS_EQUALITY_CHECK (Bool)
MYSQL_HAS_EQUALITY_CHECK (Scientific)
MYSQL_HAS_EQUALITY_CHECK (Int8)
MYSQL_HAS_EQUALITY_CHECK (Int16)
MYSQL_HAS_EQUALITY_CHECK (Int32)
MYSQL_HAS_EQUALITY_CHECK (Int64)
MYSQL_HAS_EQUALITY_CHECK (Word8)
MYSQL_HAS_EQUALITY_CHECK (Word16)
MYSQL_HAS_EQUALITY_CHECK (Word32)
MYSQL_HAS_EQUALITY_CHECK (Word64)
MYSQL_HAS_EQUALITY_CHECK (Float)
MYSQL_HAS_EQUALITY_CHECK (Double)
MYSQL_HAS_EQUALITY_CHECK (ByteString)
MYSQL_HAS_EQUALITY_CHECK (L.ByteString)
MYSQL_HAS_EQUALITY_CHECK (Text)
MYSQL_HAS_EQUALITY_CHECK (TL.Text)
MYSQL_HAS_EQUALITY_CHECK (String)
MYSQL_HAS_EQUALITY_CHECK (LocalTime)
MYSQL_HAS_EQUALITY_CHECK (Day)
MYSQL_HAS_EQUALITY_CHECK (TimeOfDay)
