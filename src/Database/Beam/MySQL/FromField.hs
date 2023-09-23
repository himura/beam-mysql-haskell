module Database.Beam.MySQL.FromField
    ( FromField (..)
    , DecodeError (..)
    , DecodeErrorDetail (..)
    ) where

import Control.Exception (Exception)
import Data.Bits (Bits (zeroBits))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Scientific (Scientific)
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
        unexpected -> handleUnexpected unexpected

instance FromField Int16 where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Int32 where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure . fromIntegral $ v
        MySQLInt32 v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Int64 where
    fromField = \case
        MySQLInt8 v -> pure . fromIntegral $ v
        MySQLInt16 v -> pure . fromIntegral $ v
        MySQLInt32 v -> pure . fromIntegral $ v
        MySQLInt64 v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Word8 where
    fromField = \case
        MySQLInt8U v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Word16 where
    fromField = \case
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Word32 where
    fromField = \case
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure . fromIntegral $ v
        MySQLInt32U v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Word64 where
    fromField = \case
        MySQLInt8U v -> pure . fromIntegral $ v
        MySQLInt16U v -> pure . fromIntegral $ v
        MySQLInt32U v -> pure . fromIntegral $ v
        MySQLInt64U v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Float where
    fromField = \case
        MySQLFloat v -> pure v
        unexpected -> handleUnexpected unexpected

instance FromField Double where
    fromField = \case
        MySQLFloat v -> pure . realToFrac $ v
        MySQLDouble v -> pure v
        unexpected -> handleUnexpected unexpected

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

handleUnexpected :: forall a. (Typeable a) => MySQLValue -> Either DecodeError a
handleUnexpected =
    Left . \case
        MySQLNull -> DecodeError tyCon DecodeErrorUnexpectedNull
        unknown -> DecodeError tyCon $ DecodeErrorTypeMismatch unknown
  where
    tyCon = typeRepTyCon $ typeRep @a
