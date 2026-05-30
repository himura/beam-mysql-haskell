module Database.Beam.MySQL.Test.Connection
    ( withConnection
    , runDB
    ) where

import Database.Beam.MySQL (MySQLM, runBeamMySQLM)
import Database.MySQL.Base qualified as MySQL
import Test.Tasty (TestTree, withResource)

withConnection :: IO MySQL.ConnectInfo -> (IO MySQL.MySQLConn -> TestTree) -> TestTree
withConnection ioConnInfo = withResource doConnect MySQL.close
  where
    doConnect = do
        connInfo <- ioConnInfo
        MySQL.connect connInfo

runDB :: MySQL.MySQLConn -> MySQLM a -> IO a
runDB conn = MySQL.withTransaction conn . runBeamMySQLM conn
