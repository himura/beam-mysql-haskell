module Database.Beam.MySQL.Test.Integration.Fixture
    ( sampleTime
    , sampleUsers
    , samplePosts
    , sampleComments
    , sampleTags
    , samplePostTags
    , insertSampleData
    ) where

import Data.Time
import Database.Beam
import Database.Beam.MySQL
import Database.Beam.MySQL.Test.Schema

sampleTime :: LocalTime
sampleTime = LocalTime (fromGregorian 2024 1 2) (TimeOfDay 3 4 5)

sampleUsers :: [User]
sampleUsers =
    [ User 1 "Alice"
    , User 2 "Bob"
    , User 3 "Charlie"
    ]

samplePosts :: [Post]
samplePosts =
    [ Post 1 (UserId 1) "Hello" "Hello, world!" sampleTime
    , Post 2 (UserId 1) "Second" "Alice's second post" sampleTime
    , Post 3 (UserId 2) "Bob's post" "Greetings from Bob" sampleTime
    ]

sampleComments :: [Comment]
sampleComments =
    [ Comment 1 (PostId 1) (UserId 2) "Nice" sampleTime
    , Comment 2 (PostId 1) (UserId 3) "Welcome" sampleTime
    , Comment 3 (PostId 2) (UserId 3) "Interesting" sampleTime
    ]

sampleTags :: [Tag]
sampleTags =
    [ Tag 1 "haskell"
    , Tag 2 "mysql"
    , Tag 3 "beam"
    ]

samplePostTags :: [PostTag]
samplePostTags =
    [ PostTag (PostId 1) (TagId 1)
    , PostTag (PostId 1) (TagId 3)
    , PostTag (PostId 2) (TagId 1)
    , PostTag (PostId 3) (TagId 2)
    ]

insertSampleData :: MySQLM ()
insertSampleData = do
    runInsert $ insert blogDb.tableUser $ insertValues sampleUsers
    runInsert $ insert blogDb.tablePost $ insertValues samplePosts
    runInsert $ insert blogDb.tableComment $ insertValues sampleComments
    runInsert $ insert blogDb.tableTag $ insertValues sampleTags
    runInsert $ insert blogDb.tablePostTag $ insertValues samplePostTags
