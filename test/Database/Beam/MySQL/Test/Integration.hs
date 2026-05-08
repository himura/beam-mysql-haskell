module Database.Beam.MySQL.Test.Integration (integrationTests) where

import Database.Beam.MySQL.Test.Integration.Crud (crudTests)
import Database.Beam.MySQL.Test.Integration.MySQLSpecific (mysqlSpecificTests)
import Database.Beam.MySQL.Test.Integration.Setup (withConnection)
import Database.MySQL.Base qualified as MySQL
import Test.Tasty
import Test.Tasty.Runners (NumThreads (..))

integrationTests :: IO MySQL.ConnectInfo -> TestTree
integrationTests ioConnInfo =
    -- Tests share a single MySQL connection (and so a single session). Run
    -- sequentially to avoid cross-talk on `last_insert_id()`, locks, and the
    -- shared row state.
    localOption (NumThreads 1) $
        withConnection ioConnInfo $ \ioConn ->
            testGroup
                "integration"
                [ crudTests ioConn
                , mysqlSpecificTests ioConnInfo ioConn
                ]
