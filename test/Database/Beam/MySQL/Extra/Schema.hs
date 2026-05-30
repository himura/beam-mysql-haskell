{-# LANGUAGE NoFieldSelectors #-}

module Database.Beam.MySQL.Extra.Schema where

import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Database.Beam
import Database.Beam.MySQL.Extra.MariaDBVector
import Database.Beam.Schema.Tables (RenamableWithRule (renamingFields))

data EmbedT f = Embed
    { id :: Columnar f Int32
    , vec :: Columnar f MariaDBVector
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type Embed = EmbedT Identity
deriving instance Show Embed
deriving instance Eq Embed

instance Table EmbedT where
    data PrimaryKey EmbedT f
        = EmbedId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = EmbedId . (.id)

newtype EmbedDb f = EmbedDb
    { tableEmbed :: f (TableEntity EmbedT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

embedDb :: DatabaseSettings be EmbedDb
embedDb =
    defaultDbSettings
        `withDbModification` renamingFields fieldNamer
  where
    fieldNamer :: NE.NonEmpty Text -> Text
    fieldNamer (n NE.:| _) = n
