{-# LANGUAGE NoFieldSelectors #-}

module StudentDB.Schema where

import Data.Bifunctor
import Data.Char
import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Database.Beam
import Database.Beam.Schema.Tables (RenamableWithRule (renamingFields))
import StudentDB.Enum

data UserT f = User
    { id :: Columnar f Int32
    , email :: Columnar f Text
    , state :: Columnar f UserStatus
    , name :: Columnar f Text
    , schoolId :: PrimaryKey SchoolT f
    , createdAt :: Columnar f LocalTime
    , updatedAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type User = UserT Identity
deriving instance Show User
deriving instance Eq User

instance Table UserT where
    data PrimaryKey UserT f
        = UserId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = UserId . (.id)
type UserId = PrimaryKey UserT Identity
deriving instance Show UserId
deriving instance Eq UserId

data SchoolT f = School
    { id :: Columnar f Int32
    , name :: Columnar f Text
    , createdAt :: Columnar f LocalTime
    , updatedAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type School = SchoolT Identity
deriving instance Show School
deriving instance Eq School

instance Table SchoolT where
    data PrimaryKey SchoolT f
        = SchoolId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = SchoolId . (.id)
type SchoolId = PrimaryKey SchoolT Identity
deriving instance Show SchoolId
deriving instance Eq SchoolId

data GroupT f = Group
    { id :: Columnar f Int32
    , name :: Columnar f Text
    , createdAt :: Columnar f LocalTime
    , updatedAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type Group = GroupT Identity
deriving instance Show Group
deriving instance Eq Group

instance Table GroupT where
    data PrimaryKey GroupT f
        = GroupId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = GroupId . (.id)
type GroupId = PrimaryKey GroupT Identity
deriving instance Show GroupId
deriving instance Eq GroupId

data UserGroupT f = UserGroup
    { userId :: PrimaryKey UserT f
    , groupId :: PrimaryKey GroupT f
    , createdAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type UserGroup = UserGroupT Identity
deriving instance Show UserGroup
deriving instance Eq UserGroup

instance Table UserGroupT where
    data PrimaryKey UserGroupT f
        = UserGroupId (PrimaryKey UserT f) (PrimaryKey GroupT f)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = UserGroupId <$> (.userId) <*> (.groupId)
type UserGroupId = PrimaryKey UserGroupT Identity
deriving instance Show UserGroupId
deriving instance Eq UserGroupId

data UserDirectoryDb f = UserDirectoryDb
    { tableUser :: f (TableEntity UserT)
    , tableSchool :: f (TableEntity SchoolT)
    , tableGroup :: f (TableEntity GroupT)
    , tableUserGroup :: f (TableEntity UserGroupT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

userDirectoryDb :: DatabaseSettings be UserDirectoryDb
userDirectoryDb =
    defaultDbSettings
        `withDbModification` renamingFields fieldNamer

fieldNamer :: NE.NonEmpty Text -> Text
fieldNamer (name NE.:| _) = unCamelCaseSel name

unCamelCase :: Text -> [Text]
unCamelCase "" = []
unCamelCase s
    | (comp, next) <- T.break isUpper s
    , not (T.null comp) =
        let next' = maybe mempty (uncurry T.cons . first toLower) (T.uncons next)
         in T.toLower comp : unCamelCase next'
    | otherwise =
        let (comp, next) = T.span isUpper s
            next' = maybe mempty (uncurry T.cons . first toLower) (T.uncons next)
         in T.toLower comp : unCamelCase next'

unCamelCaseSel :: Text -> Text
unCamelCaseSel original =
    let symbolLeft = T.dropWhile (== '_') original
     in if T.null symbolLeft
            then original
            else
                if T.any (== '_') symbolLeft
                    then symbolLeft
                    else case unCamelCase symbolLeft of
                        [] -> symbolLeft
                        [xs] -> xs
                        xs -> T.intercalate "_" xs
