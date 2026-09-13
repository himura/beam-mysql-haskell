module Database.Beam.MySQL.FullIntegrationSpec where

import Control.Exception (bracket_)
import Control.Monad (void)
import Database.Beam
import Database.Beam.MySQL.Full
import Database.Beam.MySQL.Test.Connection
import Database.Beam.MySQL.Test.Schema
import Database.Beam.MySQL.Test.ServerFlavor
import Database.MySQL.Base qualified as MySQL
import Test.Tasty
import Test.Tasty.HUnit

setupTagTable :: MySQL.MySQLConn -> IO ()
setupTagTable conn = do
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS tag"
    void $ MySQL.execute_ conn "CREATE TABLE tag (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, name TEXT NOT NULL) ENGINE=InnoDB"

teardownTagTable :: MySQL.MySQLConn -> IO ()
teardownTagTable conn =
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS tag"

withTagTable :: IO MySQL.MySQLConn -> (MySQL.MySQLConn -> Assertion) -> Assertion
withTagTable ioConn action = do
    conn <- ioConn
    bracket_ (setupTagTable conn) (teardownTagTable conn) (action conn)

-- | Locking clauses accepted by both MySQL 8.0.1+ and MariaDB 10.6+.
commonLockings :: [(String, MySQLSelectLockingStrength, Maybe MySQLSelectLockingOption)]
commonLockings =
    [ ("FOR UPDATE", MySQLSelectLockingStrengthUpdate, Nothing)
    , ("FOR UPDATE NOWAIT", MySQLSelectLockingStrengthUpdate, Just MySQLSelectLockingOptionNoWait)
    , ("FOR UPDATE SKIP LOCKED", MySQLSelectLockingStrengthUpdate, Just MySQLSelectLockingOptionSkipLocked)
    , ("LOCK IN SHARE MODE", MySQLSelectLockingStrengthLockInShareMode, Nothing)
    ]

-- | Locking clauses accepted only by MySQL.
mysqlLockings :: [(String, MySQLSelectLockingStrength, Maybe MySQLSelectLockingOption)]
mysqlLockings =
    [ ("FOR SHARE", MySQLSelectLockingStrengthShare, Nothing)
    , ("FOR SHARE NOWAIT", MySQLSelectLockingStrengthShare, Just MySQLSelectLockingOptionNoWait)
    , ("FOR SHARE SKIP LOCKED", MySQLSelectLockingStrengthShare, Just MySQLSelectLockingOptionSkipLocked)
    ]

-- | Locking clauses accepted only by MariaDB.
mariadbLockings :: [(String, MySQLSelectLockingStrength, Maybe MySQLSelectLockingOption)]
mariadbLockings =
    [ ("FOR UPDATE WAIT 1", MySQLSelectLockingStrengthUpdate, Just (MySQLSelectLockingOptionWait 1))
    , ("LOCK IN SHARE MODE NOWAIT", MySQLSelectLockingStrengthLockInShareMode, Just MySQLSelectLockingOptionNoWait)
    , ("LOCK IN SHARE MODE SKIP LOCKED", MySQLSelectLockingStrengthLockInShareMode, Just MySQLSelectLockingOptionSkipLocked)
    , ("LOCK IN SHARE MODE WAIT 1", MySQLSelectLockingStrengthLockInShareMode, Just (MySQLSelectLockingOptionWait 1))
    ]

integrationTests :: IO MySQL.ConnectInfo -> TestTree
integrationTests ioConnInfo = askOption $ \flavor ->
    let lockings = case flavor of
            MySQLServer -> commonLockings <> mysqlLockings
            MariaDBServer -> commonLockings <> mariadbLockings
     in withConnection ioConnInfo $ \ioConn ->
            -- These tests share a single 'tag' table that each test
            -- drop/creates, so they must not run concurrently.
            dependentTestGroup
                "locking reads"
                AllFinish
                (map (lockingTestCase ioConn) lockings)

lockingTestCase
    :: IO MySQL.MySQLConn
    -> (String, MySQLSelectLockingStrength, Maybe MySQLSelectLockingOption)
    -> TestTree
lockingTestCase ioConn (name, strength, option) =
    testCase name $
        withTagTable ioConn $ \conn -> do
            runDB conn $
                runInsert $
                    insert blogDb.tableTag $
                        insertValues
                            [ Tag 1 "tag1"
                            , Tag 2 "tag2"
                            ]

            tags <-
                runDB conn $
                    runSelectReturningList $
                        select $
                            lockingFor_ strength option $
                                orderBy_ (\tag -> asc_ tag.id) $
                                    all_ blogDb.tableTag
            tags @?= [Tag 1 "tag1", Tag 2 "tag2"]
