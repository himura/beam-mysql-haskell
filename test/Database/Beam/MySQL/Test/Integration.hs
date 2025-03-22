module Database.Beam.MySQL.Test.Integration where

import Control.Exception
import Control.Monad
import Database.Beam
import Database.Beam.MySQL
import Database.Beam.MySQL.Test.Schema
import Database.MySQL.Base qualified as MySQL
import Test.Tasty
import Test.Tasty.HUnit

withConnection :: IO MySQL.ConnectInfo -> (IO MySQL.MySQLConn -> TestTree) -> TestTree
withConnection ioConnInfo = withResource doConnect MySQL.close
  where
    doConnect = do
        connInfo <- ioConnInfo
        MySQL.connect connInfo

setup :: IO MySQL.MySQLConn -> IO MySQL.MySQLConn
setup ioConn = do
    conn <- ioConn
    teardown conn
    void $ MySQL.execute_ conn "CREATE TABLE user (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, name TEXT NOT NULL)"
    void $ MySQL.execute_ conn "CREATE TABLE post (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, author_id INTEGER NOT NULL, title TEXT NOT NULL, body TEXT NOT NULL, created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    void $ MySQL.execute_ conn "CREATE TABLE comment (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, post_id INTEGER NOT NULL, author_id INTEGER NOT NULL, body TEXT NOT NULL, created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    return conn

teardown :: MySQL.MySQLConn -> IO ()
teardown conn = do
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS comment"
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS post"
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS user"

withTestDB :: IO MySQL.MySQLConn -> (MySQL.MySQLConn -> Assertion) -> Assertion
withTestDB ioConn = bracket (setup ioConn) teardown

runDB :: MySQL.MySQLConn -> MySQLM a -> IO a
runDB conn = MySQL.withTransaction conn . runBeamMySQLM conn

integrationTests :: IO MySQL.ConnectInfo -> TestTree
integrationTests ioConnInfo =
    withConnection ioConnInfo $ \ioConn ->
        testGroup
            "blog example"
            [ testCase "insert & select" $ withTestDB ioConn $ \conn -> do
                runDB conn $ do
                    runInsert $
                        insert blogDb.tableUser $
                            insertValues
                                [ User 1 "user1"
                                , User 2 "user2"
                                ]

                users <- runDB conn $ do
                    runSelectReturningList $
                        select $ do
                            user <- all_ blogDb.tableUser
                            guard_ $ user.id ==. 2
                            return user

                users @?= [User 2 "user2"]
            ]
