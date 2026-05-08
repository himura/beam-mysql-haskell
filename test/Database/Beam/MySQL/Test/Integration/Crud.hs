module Database.Beam.MySQL.Test.Integration.Crud (crudTests) where

import Data.Int (Int32)
import Data.Maybe (isJust)
import Database.Beam
import Database.Beam.MySQL
import Database.Beam.MySQL.Test.Integration.Fixture
import Database.Beam.MySQL.Test.Integration.Setup
import Database.Beam.MySQL.Test.Schema
import Database.MySQL.Base qualified as MySQL
import Test.Tasty
import Test.Tasty.HUnit

crudTests :: IO MySQL.MySQLConn -> TestTree
crudTests ioConn =
    testGroup
        "CRUD"
        [ insertTests ioConn
        , selectTests ioConn
        , updateTests ioConn
        , deleteTests ioConn
        ]

insertTests :: IO MySQL.MySQLConn -> TestTree
insertTests ioConn =
    testGroup
        "insert"
        [ testCase "insertValues round-trips" $ withCleanTables ioConn $ \conn -> do
            runDB conn $ runInsert $ insert blogDb.tableUser $ insertValues sampleUsers
            users <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            orderBy_ (\u -> asc_ u.id) $
                                all_ blogDb.tableUser
            users @?= sampleUsers
        , testCase "insertExpressions with default_ uses auto_increment" $ withCleanTables ioConn $ \conn -> do
            runDB conn $
                runInsert $
                    insert blogDb.tableUser $
                        insertExpressions
                            [ User default_ (val_ "Alice")
                            , User default_ (val_ "Bob")
                            ]
            users <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            orderBy_ (\u -> asc_ u.id) $
                                all_ blogDb.tableUser
            map (.name) users @?= ["Alice", "Bob"]
            length users @?= 2
            assertBool "ids must be positive" $ all (\u -> u.id > 0) users
        , testCase "insertOnly + insertFrom copies columns" $ withCleanTables ioConn $ \conn -> do
            runDB conn $ do
                runInsert $ insert blogDb.tableUser $ insertValues sampleUsers
                runInsert $ insert blogDb.tablePost $ insertValues samplePosts
                runInsert $ insert blogDb.tableTag $ insertValues sampleTags
            -- Tag every post authored by Alice (id=1) with "haskell" (id=1).
            runDB conn $
                runInsert $
                    insertOnly blogDb.tablePostTag (\pt -> (pt.postId, pt.tagId)) $
                        insertFrom $ do
                            post <- all_ blogDb.tablePost
                            guard_ $ post.authorId ==. val_ (UserId 1)
                            return (pk post, TagId (val_ 1) :: PrimaryKey TagT (QExpr MySQL QBaseScope))
            pts <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            orderBy_ (\pt -> let PostId p = pt.postId in asc_ p) $
                                all_ blogDb.tablePostTag
            pts
                @?= [ PostTag (PostId 1) (TagId 1)
                    , PostTag (PostId 2) (TagId 1)
                    ]
        ]

selectTests :: IO MySQL.MySQLConn -> TestTree
selectTests ioConn =
    testGroup
        "select"
        [ testCase "where with multi-condition" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            posts <- runDB conn $
                runSelectReturningList $
                    select $ do
                        post <- all_ blogDb.tablePost
                        guard_ $ post.authorId ==. val_ (UserId 1) &&. post.title `like_` val_ "S%"
                        return post
            map (.title) posts @?= ["Second"]
        , testCase "inner join via related_" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            rows <- runDB conn $
                runSelectReturningList $
                    select $
                        orderBy_ (\(p, _) -> asc_ p.id) $ do
                            post <- all_ blogDb.tablePost
                            author <- related_ blogDb.tableUser post.authorId
                            return (post, author)
            map (\(p, u) -> (p.title, u.name)) rows
                @?= [("Hello", "Alice"), ("Second", "Alice"), ("Bob's post", "Bob")]
        , testCase "left join counts posts per user including zero" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            counts <- runDB conn $
                runSelectReturningList $
                    select $
                        orderBy_ (\(uid, _) -> asc_ uid) $
                            aggregate_ (\(user, mPost) -> (group_ user.id, as_ @Int32 $ count_ mPost.id)) $ do
                                user <- all_ blogDb.tableUser
                                mPost <- leftJoin_ (all_ blogDb.tablePost) (\p -> p.authorId ==. pk user)
                                return (user, mPost)
            counts @?= [(1, 2), (2, 1), (3, 0)]
        , testCase "orderBy + limit + offset" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            page <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            limit_ 2 $
                                offset_ 1 $
                                    orderBy_ (\u -> asc_ u.id) $
                                        all_ blogDb.tableUser
            map (.name) page @?= ["Bob", "Charlie"]
        , testCase "aggregate with having" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            rows <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            filter_ (\(_, n) -> n >=. val_ 2) $
                                aggregate_ (\post -> (group_ post.authorId, as_ @Int32 countAll_)) $
                                    all_ blogDb.tablePost
            rows @?= [(UserId 1, 2)]
        , testCase "union of post and comment authors" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            rows <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            orderBy_ asc_ $
                                (do post <- all_ blogDb.tablePost; let UserId aid = post.authorId in return aid)
                                    `union_` (do c <- all_ blogDb.tableComment; let UserId aid = c.authorId in return aid)
            rows @?= [1 :: Int32, 2, 3]
        , testCase "exists_ subquery" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            users <- runDB conn $
                runSelectReturningList $
                    select $
                        orderBy_ (\u -> asc_ u.id) $ do
                            user <- all_ blogDb.tableUser
                            guard_ $
                                exists_ $ do
                                    post <- all_ blogDb.tablePost
                                    guard_ $ post.authorId ==. pk user
                                    return post
                            return user
            map (.name) users @?= ["Alice", "Bob"]
        , testCase "in_ list filter" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            users <- runDB conn $
                runSelectReturningList $
                    select $
                        orderBy_ (\u -> asc_ u.id) $ do
                            user <- all_ blogDb.tableUser
                            guard_ $ user.id `in_` [val_ 1, val_ 3]
                            return user
            map (.name) users @?= ["Alice", "Charlie"]
        ]

updateTests :: IO MySQL.MySQLConn -> TestTree
updateTests ioConn =
    testGroup
        "update"
        [ testCase "save replaces row" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            runDB conn $ runUpdate $ save blogDb.tableUser (User 2 "Robert")
            mUser <- runDB conn $ runSelectReturningOne $ lookup_ blogDb.tableUser (UserId 2)
            mUser @?= Just (User 2 "Robert")
        , testCase "update sets a single field" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            runDB conn $
                runUpdate $
                    update
                        blogDb.tableUser
                        (\u -> u.name <-. val_ "Alice (renamed)")
                        (\u -> u.id ==. val_ 1)
            mUser <- runDB conn $ runSelectReturningOne $ lookup_ blogDb.tableUser (UserId 1)
            fmap (.name) mUser @?= Just "Alice (renamed)"
            mBob <- runDB conn $ runSelectReturningOne $ lookup_ blogDb.tableUser (UserId 2)
            fmap (.name) mBob @?= Just "Bob"
        ]

deleteTests :: IO MySQL.MySQLConn -> TestTree
deleteTests ioConn =
    testGroup
        "delete"
        [ testCase "delete with filter" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            runDB conn $
                runDelete $
                    delete blogDb.tablePost (\p -> p.authorId ==. val_ (UserId 1))
            posts <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            orderBy_ (\p -> asc_ p.id) $
                                all_ blogDb.tablePost
            map (.title) posts @?= ["Bob's post"]
        , testCase "delete all rows" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            runDB conn $ runDelete $ delete blogDb.tableComment (\_ -> val_ True)
            cs <- runDB conn $ runSelectReturningList $ select $ all_ blogDb.tableComment
            cs @?= []
            us <- runDB conn $ runSelectReturningList $ select $ all_ blogDb.tableUser
            length us @?= 3
        , testCase "lookup_ after partial delete" $ withCleanTables ioConn $ \conn -> do
            runDB conn insertSampleData
            runDB conn $ runDelete $ delete blogDb.tableUser (\u -> u.id ==. val_ 3)
            mUser <- runDB conn $ runSelectReturningOne $ lookup_ blogDb.tableUser (UserId 3)
            assertBool "user 3 must be gone" (not (isJust mUser))
        ]
