{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Beam.MySQL.FromField
    ( FromField (..)
    , DecodeError (..)
    , DecodeErrorDetail (..)
    ) where

import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.Bits (Bits (zeroBits), toIntegralSized)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Scientific (Scientific, fromFloatDigits, toBoundedInteger, toBoundedRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (Day, LocalTime (LocalTime), TimeOfDay, midnight)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam (Generic, Typeable)
import Database.Beam.Backend (SqlNull (..))
import Database.MySQL.Base (MySQLValue (..))
import Type.Reflection (TyCon, typeRep, typeRepTyCon)

class FromField a where
    fromField :: MySQLValue -> Either DecodeError a

data DecodeErrorDetail
    = DecodeErrorUnexpectedNull
    | DecodeErrorTypeMismatch MySQLValue
    | DecodeErrorOverflow MySQLValue
    deriving stock (Generic, Eq, Show)

data DecodeError = DecodeError
    { decodeErrorHaskellType :: TyCon
    , decodeErrorDetail :: DecodeErrorDetail
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Exception)

instance FromField SqlNull where
    fromField = \case
        MySQLNull -> Right SqlNull
        unexpected -> handleUnexpected unexpected

instance FromField Bool where
    fromField = \case
        MySQLInt8 v -> pure $ zeroBits /= v
        MySQLInt8U v -> pure $ zeroBits /= v
        MySQLBit v -> pure $ zeroBits /= v
        unexpected -> handleUnexpected unexpected

instance FromField Scientific where
    fromField = \case
        MySQLDecimal v -> pure v
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure . fromIntegral $ v
        MySQLInt32U v -> pure . fromIntegral $ v
        MySQLInt64U v -> pure . fromIntegral $ v
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure . fromIntegral $ v
        MySQLInt32 v -> pure . fromIntegral $ v
        MySQLInt64 v -> pure . fromIntegral $ v
        unexpected -> handleUnexpected unexpected

instance FromField Int8 where
    fromField = \case
        MySQLInt8 v -> pure v
        others -> tryBoundedIntegral others

instance FromField Int16 where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure v
        others -> tryBoundedIntegral others

instance FromField Int32 where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure . fromIntegral $ v
        MySQLInt32 v -> pure v
        others -> tryBoundedIntegral others

instance FromField Int64 where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure . fromIntegral $ v
        MySQLInt32 v -> pure . fromIntegral $ v
        MySQLInt64 v -> pure v
        others -> tryBoundedIntegral others

-- | This instance does not support conversion from MySQLDecimal(Scientific)
--   because the destination `Integer` type is unbounded. Allowing such a conversion
--   could result in excessive memory usage if malicious input with huge exponents is provided.
instance FromField Integer where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure . fromIntegral $ v
        MySQLInt32 v -> pure . fromIntegral $ v
        MySQLInt64 v -> pure . fromIntegral $ v
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure . fromIntegral $ v
        MySQLInt32U v -> pure . fromIntegral $ v
        MySQLInt64U v -> pure . fromIntegral $ v
        unexpected -> handleUnexpected unexpected

instance FromField Word8 where
    fromField = \case
        MySQLInt8U v -> pure v
        others -> tryBoundedIntegral others

instance FromField Word16 where
    fromField = \case
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure v
        others -> tryBoundedIntegral others

instance FromField Word32 where
    fromField = \case
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure . fromIntegral $ v
        MySQLInt32U v -> pure v
        others -> tryBoundedIntegral others

instance FromField Word64 where
    fromField = \case
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure . fromIntegral $ v
        MySQLInt32U v -> pure . fromIntegral $ v
        MySQLInt64U v -> pure v
        others -> tryBoundedIntegral others

instance FromField Float where
    fromField = \case
        MySQLFloat v -> pure v
        others -> tryBoundedRealFloat others

instance FromField Double where
    fromField = \case
        MySQLFloat v -> pure . realToFrac $ v
        MySQLDouble v -> pure v
        others -> tryBoundedRealFloat others

instance FromField ByteString where
    fromField = \case
        MySQLBytes v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField L.ByteString where
    fromField = \case
        MySQLBytes v -> pure . L.fromStrict $ v
        unexpected -> handleUnexpected unexpected

instance FromField Text where
    fromField = \case
        MySQLText v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField TL.Text where
    fromField = \case
        MySQLText v -> pure . TL.fromStrict $ v
        unexpected -> handleUnexpected unexpected

instance FromField String where
    fromField = \case
        MySQLText v -> pure . T.unpack $ v
        unexpected -> handleUnexpected unexpected

instance FromField LocalTime where
    fromField = \case
        MySQLDateTime v -> pure v
        MySQLTimeStamp v -> pure v
        MySQLDate v -> pure . LocalTime v $ midnight
        unexpected -> handleUnexpected unexpected

instance FromField Day where
    fromField = \case
        MySQLDate v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField TimeOfDay where
    fromField = \case
        MySQLTime s v
            | s == zeroBits -> pure v
        unexpected -> handleUnexpected unexpected

tyCon :: forall (a :: Type). (Typeable a) => TyCon
tyCon = typeRepTyCon $ typeRep @a

tryBoundedIntegral
    :: forall a
     . (Integral a, Bounded a, Bits a, Typeable a)
    => MySQLValue
    -> Either DecodeError a
tryBoundedIntegral mv =
    case mv of
        MySQLDecimal v -> maybe overflow pure $ toBoundedInteger v
        MySQLInt8U v -> tryInt v
        MySQLInt16U v -> tryInt v
        MySQLInt32U v -> tryInt v
        MySQLInt64U v -> tryInt v
        MySQLInt8 v -> tryInt v
        MySQLInt16 v -> tryInt v
        MySQLInt32 v -> tryInt v
        MySQLInt64 v -> tryInt v
        unexpected -> handleUnexpected unexpected
  where
    overflow = Left . DecodeError (tyCon @a) $ DecodeErrorOverflow mv

    tryInt :: forall b. (Integral b, Bits b) => b -> Either DecodeError a
    tryInt = maybe overflow pure . toIntegralSized

tryBoundedRealFloat :: forall a. (RealFloat a, Typeable a) => MySQLValue -> Either DecodeError a
tryBoundedRealFloat mv =
    case mv of
        MySQLFloat v -> tryRealFloat $ fromFloatDigits v
        MySQLDouble v -> tryRealFloat $ fromFloatDigits v
        MySQLDecimal v -> tryRealFloat v
        unexpected -> handleUnexpected unexpected
  where
    tryRealFloat :: Scientific -> Either DecodeError a
    tryRealFloat = first (\_ -> DecodeError (tyCon @a) (DecodeErrorOverflow mv)) . toBoundedRealFloat

handleUnexpected :: forall a. (Typeable a) => MySQLValue -> Either DecodeError a
handleUnexpected =
    Left . \case
        MySQLNull -> DecodeError (tyCon @a) DecodeErrorUnexpectedNull
        unknown -> DecodeError (tyCon @a) $ DecodeErrorTypeMismatch unknown
