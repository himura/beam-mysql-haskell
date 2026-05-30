module Database.Beam.MySQL.Extra.MariaDBVectorIntegrationSpec where

import Control.Exception (bracket_)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Beam
import Database.Beam.MySQL
import Database.Beam.MySQL.Extra.MariaDBVector
import Database.Beam.MySQL.Extra.Schema
import Database.Beam.MySQL.Test.Connection
import Database.Beam.MySQL.Test.ServerFlavor
import Database.MySQL.Base qualified as MySQL
import Test.Tasty
import Test.Tasty.HUnit

setupEmbedTable :: MySQL.MySQLConn -> IO ()
setupEmbedTable conn = do
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS embed"
    void $
        MySQL.execute_
            conn
            "CREATE TABLE embed (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, vec VECTOR(3) NOT NULL)"

teardownEmbedTable :: MySQL.MySQLConn -> IO ()
teardownEmbedTable conn =
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS embed"

withEmbedTable :: IO MySQL.MySQLConn -> (MySQL.MySQLConn -> Assertion) -> Assertion
withEmbedTable ioConn action = do
    conn <- ioConn
    bracket_ (setupEmbedTable conn) (teardownEmbedTable conn) (action conn)

integrationTests :: IO MySQL.ConnectInfo -> TestTree
integrationTests ioConnInfo = askOption $ \case
    MariaDBServer -> mariadbIntegrationTests ioConnInfo
    MySQLServer -> testGroup "MariaDBVector" []

mariadbIntegrationTests :: IO MySQL.ConnectInfo -> TestTree
mariadbIntegrationTests ioConnInfo =
    withConnection ioConnInfo $ \ioConn ->
        -- These tests share a single 'embed' table that each test
        -- drop/creates, so they must not run concurrently.
        dependentTestGroup
            "MariaDBVector (requires MariaDB 11.7+)"
            AllFinish
            [ testCase "INSERT and SELECT round-trip preserves the vector" $
                withEmbedTable ioConn $ \conn -> do
                    let v1 = fromFloats [0.1, 0.2, 0.3]
                        v2 = fromFloats [1.0, 2.0, 3.0]
                    runDB conn $
                        runInsert $
                            insert embedDb.tableEmbed $
                                insertValues
                                    [ Embed 1 v1
                                    , Embed 2 v2
                                    ]

                    rows <-
                        runDB conn $
                            runSelectReturningList $
                                select $
                                    orderBy_ (\e -> asc_ e.id) $
                                        all_ embedDb.tableEmbed
                    rows @?= [Embed 1 v1, Embed 2 v2]
            , testCase "vecDistanceEuclidean_ orders rows by distance" $
                withEmbedTable ioConn $ \conn -> do
                    runDB conn $
                        runInsert $
                            insert embedDb.tableEmbed $
                                insertValues
                                    [ Embed 1 (fromFloats [0.0, 0.0, 0.0])
                                    , Embed 2 (fromFloats [10.0, 0.0, 0.0])
                                    , Embed 3 (fromFloats [1.0, 0.0, 0.0])
                                    ]

                    let target = val_ (fromFloats [0.0, 0.0, 0.0])
                    ids <-
                        runDB conn $
                            runSelectReturningList $
                                select $
                                    fmap (.id) $
                                        orderBy_ (\e -> asc_ (vecDistanceEuclidean_ e.vec target)) $
                                            all_ embedDb.tableEmbed
                    ids @?= [1, 3, 2]
            , testCase "vecFromText_ parses JSON array literal" $
                withEmbedTable ioConn $ \conn -> do
                    runDB conn $
                        runInsert $
                            insert embedDb.tableEmbed $
                                insertExpressions
                                    [ Embed (val_ 1) (vecFromText_ (val_ ("[0.5, 1.5, 2.5]" :: Text)))
                                    ]
                    rows <-
                        runDB conn $
                            runSelectReturningList $
                                select $
                                    all_ embedDb.tableEmbed
                    rows @?= [Embed 1 (fromFloats [0.5, 1.5, 2.5])]
            , testCase "vecToText_ renders vector as JSON array" $
                withEmbedTable ioConn $ \conn -> do
                    runDB conn $
                        runInsert $
                            insert embedDb.tableEmbed $
                                insertValues [Embed 1 (fromFloats [1.0, 2.0, 3.0])]

                    -- VEC_ToText returns "[1.000000,2.000000,3.000000]" in
                    -- current MariaDB versions; check key markers rather than
                    -- exact formatting.
                    texts <- runDB conn $
                        runSelectReturningList $
                            select $ do
                                e <- all_ embedDb.tableEmbed
                                return (vecToText_ e.vec :: QGenExpr QValueContext MySQL QBaseScope Text)
                    case texts of
                        [t] -> do
                            assertBool ("text starts with [: " <> show t) ("[" `isPrefixOf'` t)
                            assertBool ("text ends with ]: " <> show t) ("]" `isSuffixOf'` t)
                        _ -> assertFailure $ "expected exactly one row, got: " <> show texts
            ]
  where
    isPrefixOf' p t = case T.stripPrefix p t of
        Just _ -> True
        Nothing -> False
    isSuffixOf' s t = case T.stripSuffix s t of
        Just _ -> True
        Nothing -> False
