module Database.Beam.MySQL.Test.Integration.Setup
    ( withConnection
    , withCleanTables
    , runDB
    , recreateSchema
    , clearTables
    ) where

import Control.Monad
import Database.Beam.MySQL
import Database.MySQL.Base qualified as MySQL
import Test.Tasty
import Test.Tasty.HUnit

-- | Open a single MySQL connection for the wrapped 'TestTree' and ensure the
-- schema is in place. The connection is reused across tests; per-test
-- isolation is provided by 'withCleanTables'.
withConnection :: IO MySQL.ConnectInfo -> (IO MySQL.MySQLConn -> TestTree) -> TestTree
withConnection ioConnInfo = withResource doConnect MySQL.close
  where
    doConnect = do
        connInfo <- ioConnInfo
        conn <- MySQL.connect connInfo
        recreateSchema conn
        return conn

-- | Drop and recreate every table the integration tests use. Called once per
-- suite via 'withConnection'.
recreateSchema :: MySQL.MySQLConn -> IO ()
recreateSchema conn = do
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS post_tag"
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS tag"
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS comment"
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS post"
    void $ MySQL.execute_ conn "DROP TABLE IF EXISTS user"
    void $ MySQL.execute_ conn "CREATE TABLE user (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, name TEXT NOT NULL)"
    void $ MySQL.execute_ conn "CREATE TABLE post (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, author_id INTEGER NOT NULL, title TEXT NOT NULL, body TEXT NOT NULL, created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    void $ MySQL.execute_ conn "CREATE TABLE comment (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, post_id INTEGER NOT NULL, author_id INTEGER NOT NULL, body TEXT NOT NULL, created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    void $ MySQL.execute_ conn "CREATE TABLE tag (id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT, name TEXT NOT NULL)"
    void $ MySQL.execute_ conn "CREATE TABLE post_tag (post_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, PRIMARY KEY (post_id, tag_id))"

-- | Empty every table without dropping it. Used at the start of each test so
-- the suite avoids per-test DDL.
clearTables :: MySQL.MySQLConn -> IO ()
clearTables conn = do
    void $ MySQL.execute_ conn "DELETE FROM post_tag"
    void $ MySQL.execute_ conn "DELETE FROM tag"
    void $ MySQL.execute_ conn "DELETE FROM comment"
    void $ MySQL.execute_ conn "DELETE FROM post"
    void $ MySQL.execute_ conn "DELETE FROM user"

withCleanTables :: IO MySQL.MySQLConn -> (MySQL.MySQLConn -> Assertion) -> Assertion
withCleanTables ioConn action = do
    conn <- ioConn
    clearTables conn
    action conn

runDB :: MySQL.MySQLConn -> MySQLM a -> IO a
runDB conn = MySQL.withTransaction conn . runBeamMySQLM conn
