{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module StudentDB.Enum
    ( DBEnumNumeric (..)
    , UserStatus (..)
    ) where

import Data.Int
import Database.Beam
import Database.Beam.Backend

newtype DBEnumNumeric a = DBEnumNumeric {unDBEnumNumeric :: a}

instance (Enum e, HasSqlValueSyntax be Int32) => HasSqlValueSyntax be (DBEnumNumeric e) where
    sqlValueSyntax = enumToSqlValueSyntax . unDBEnumNumeric
instance (Enum e, FromBackendRow be Int32) => FromBackendRow be (DBEnumNumeric e) where
    fromBackendRow = DBEnumNumeric <$> enumFromBackendRow

enumToSqlValueSyntax :: (Enum e, HasSqlValueSyntax be Int32) => e -> be
enumToSqlValueSyntax = sqlValueSyntax . fromIntegral @Int @Int32 . fromEnum

enumFromBackendRow :: (FromBackendRow be Int32, Enum e) => FromBackendRowM be e
enumFromBackendRow = toEnum . fromIntegral @Int32 @Int <$> fromBackendRow

data UserStatus
    = UserActive
    | UserInactive
    | UserNotImplemented
    deriving stock (Generic, Ord, Eq, Enum, Show, Read)
deriving via DBEnumNumeric UserStatus instance (HasSqlValueSyntax be Int32) => (HasSqlValueSyntax be UserStatus)
deriving via DBEnumNumeric UserStatus instance (BeamBackend be, FromBackendRow be Int32) => FromBackendRow be UserStatus

-- Wredundant-constraint notes that `BeamBackend be` constraint is redundant, which is incorrect
instance (BeamSqlBackend be) => HasSqlEqualityCheck be UserStatus
