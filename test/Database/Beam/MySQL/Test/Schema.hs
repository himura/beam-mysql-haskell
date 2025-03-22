{-# LANGUAGE NoFieldSelectors #-}

module Database.Beam.MySQL.Test.Schema where

import Data.Bifunctor
import Data.Char
import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Database.Beam
import Database.Beam.Schema.Tables (RenamableWithRule (renamingFields))

data UserT f = User
    { id :: Columnar f Int32
    , name :: Columnar f Text
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

data PostT f = Post
    { id :: Columnar f Int32
    , authorId :: PrimaryKey UserT f
    , title :: Columnar f Text
    , body :: Columnar f Text
    , createdAt :: Columnar f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type Post = PostT Identity
deriving instance Show Post
deriving instance Eq Post

instance Table PostT where
    data PrimaryKey PostT f
        = PostId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = PostId . (.id)
type PostId = PrimaryKey PostT Identity
deriving instance Show PostId
deriving instance Eq PostId

data CommentT f = Comment
    { id :: Columnar f Int32
    , postId :: PrimaryKey PostT f
    , authorId :: PrimaryKey UserT f
    , body :: Columnar f Text
    , createdAt :: Columnar f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type Comment = CommentT Identity
deriving instance Show Comment
deriving instance Eq Comment

instance Table CommentT where
    data PrimaryKey CommentT f
        = CommentId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = CommentId . (.id)
type CommentId = PrimaryKey CommentT Identity
deriving instance Show CommentId
deriving instance Eq CommentId

data TagT f = Tag
    { id :: Columnar f Int32
    , name :: Columnar f Text
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type Tag = TagT Identity
deriving instance Show Tag
deriving instance Eq Tag

instance Table TagT where
    data PrimaryKey TagT f
        = TagId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = TagId . (.id)
type TagId = PrimaryKey TagT Identity
deriving instance Show TagId
deriving instance Eq TagId

data PostTagT f = PostTag
    { postId :: PrimaryKey PostT f
    , tagId :: PrimaryKey TagT f
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
type PostTag = PostTagT Identity
deriving instance Show PostTag
deriving instance Eq PostTag

instance Table PostTagT where
    data PrimaryKey PostTagT f
        = PostTagId (PrimaryKey PostT f) (PrimaryKey TagT f)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = PostTagId <$> (.postId) <*> (.tagId)

data BlogDb f = BlogDb
    { tableUser :: f (TableEntity UserT)
    , tablePost :: f (TableEntity PostT)
    , tableComment :: f (TableEntity CommentT)
    , tableTag :: f (TableEntity TagT)
    , tablePostTag :: f (TableEntity PostTagT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

blogDb :: DatabaseSettings be BlogDb
blogDb =
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
