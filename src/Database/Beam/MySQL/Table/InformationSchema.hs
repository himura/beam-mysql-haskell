{-# LANGUAGE NoFieldSelectors #-}

module Database.Beam.MySQL.Table.InformationSchema
    ( ColumnsT (..)
    , PrimaryKey (..)
    , InformationSchemaDb (..)
    , informationSchemaDb
    ) where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Database.Beam
    ( Beamable
    , Columnar
    , Database
    , DatabaseSettings
    , Generic
    , Table (..)
    , TableEntity
    , defaultDbSettings
    , withDbModification
    )
import Database.Beam.Schema.Tables
    ( Database (zipTables)
    , RenamableWithRule (renamingFields)
    , setEntitySchema
    )

data ColumnsT f = Columns
    { tableCatalog :: Columnar f Text
    , tableSchema :: Columnar f Text
    , tableName :: Columnar f Text
    , columnName :: Columnar f Text
    , extra :: Columnar f Text
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table ColumnsT where
    data PrimaryKey ColumnsT f = ColumnsPK (Columnar f Text) (Columnar f Text) (Columnar f Text) (Columnar f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)

    primaryKey = ColumnsPK <$> (.tableCatalog) <*> (.tableSchema) <*> (.tableName) <*> (.columnName)

newtype InformationSchemaDb f = InformationSchemaDb
    { tableColumns :: f (TableEntity ColumnsT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

informationSchemaDb :: forall be. DatabaseSettings be InformationSchemaDb
informationSchemaDb = defaultDbSettings `withDbModification` setEntitySchemaAll
  where
    setEntitySchemaAll =
        runIdentity $
            zipTables
                (Proxy @be)
                (\_ _ -> pure (setEntitySchema (Just "information_schema") <> renamingFields fieldNamer))
                (undefined :: DatabaseSettings be InformationSchemaDb)
                (undefined :: DatabaseSettings be InformationSchemaDb)

fieldNamer :: NE.NonEmpty Text -> Text
fieldNamer (name NE.:| _) = T.intercalate "_" $ map T.toUpper $ unCamelCase name

unCamelCase :: Text -> [Text]
unCamelCase "" = []
unCamelCase s
    | (comp, next) <- T.break isUpper s
    , not (T.null comp) =
        let next' = maybe mempty (uncurry T.cons . first toLower) (T.uncons next)
         in comp : unCamelCase next'
    | otherwise =
        let (comp, next) = T.span isUpper s
            next' = maybe mempty (uncurry T.cons . first toLower) (T.uncons next)
         in comp : unCamelCase next'
