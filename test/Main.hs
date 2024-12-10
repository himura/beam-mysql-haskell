{-# LANGUAGE CPP #-}

module Main where

import Control.Exception
import Control.Monad
import Database.Beam.MySQL.Syntax.Spec qualified
import Database.Beam.MySQL.Test.Integration qualified
import Database.MySQL.Base (ConnectInfo (..))
import Database.MySQL.Base qualified as MySQL
import Test.Tasty as Tasty
#ifdef USE_TEST_CONTAINER
import Data.String
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import TestContainers.Tasty qualified as TC
import Control.Concurrent
import Control.Monad.IO.Class
#else
import Data.ByteString.Char8 qualified as S8
import Data.Maybe
import Data.String
import System.Environment
#endif

main :: IO ()
main = do
    defaultMain allTests

allTests :: TestTree
allTests =
    testGroup
        "beam-mysql-haskell"
        [ tests
        , withTestDB integrationTests
        ]

tests :: TestTree
tests =
    testGroup
        "unit Database.Beam.MySQL"
        [ Database.Beam.MySQL.Syntax.Spec.tests
        ]

integrationTests :: IO MySQL.ConnectInfo -> TestTree
integrationTests ioConnInfo =
    testGroup
        "integration"
        [ Database.Beam.MySQL.Test.Integration.integrationTests ioConnInfo
        ]

#ifdef USE_TEST_CONTAINER

-- TODO: Exception: Short read, expected 4 bytes

setupTemporaryDB :: (TC.MonadDocker m) => m MySQL.ConnectInfo
setupTemporaryDB = do
    let user = "test"
        password = "test"
        database = "test"

    tc <-
        TC.run $
            TC.containerRequest (TC.fromTag "mysql:8.0-debian")
                TC.& TC.setExpose [3306]
                TC.& TC.setEnv
                    [ ("MYSQL_USER", user)
                    , ("MYSQL_PASSWORD", password)
                    , ("MYSQL_DATABASE", database)
                    , ("MYSQL_ROOT_PASSWORD", password)
                    ]
                TC.& TC.setCmd ["--default-authentication-plugin=mysql_native_password"]
                TC.& (TC.setWaitingFor $
                        mconcat [ TC.waitForLogLine TC.Stdout (TL.isInfixOf "init process done. Ready for start up.")
                                , TC.waitForLogLine TC.Stderr (TL.isInfixOf "ready for connections.")
                                , TC.waitUntilTimeout 120 $ TC.waitUntilMappedPortReachable 3306
                        ])

    let connectInfo =
         MySQL.defaultConnectInfoMB4
            { ciUser = T.encodeUtf8 user
            , ciPassword = T.encodeUtf8 password
            , ciDatabase = T.encodeUtf8 database
            , ciHost = "127.0.0.1"
            , ciPort = fromIntegral $ TC.containerPort tc 3306
            }

    liftIO $ waitConnection 5 connectInfo

    return connectInfo

waitConnection
    :: Int -- ^ max tries
    -> MySQL.ConnectInfo
    -> IO ()
waitConnection maxAttempts connectInfo =
    loop 0
  where
    testConnection = do
        bracket (MySQL.connect connectInfo) MySQL.close $ \conn -> do
            void $ MySQL.query_ conn (fromString "SELECT 1")
    loop n
        | n < maxAttempts = do
              testConnection `catch` \(SomeException exc) -> do
                  print exc
                  threadDelay $ 1000 * 500
                  loop $ n + 1
        | otherwise = return ()

withTestDB :: (IO MySQL.ConnectInfo -> TestTree) -> TestTree
withTestDB = TC.withContainers setupTemporaryDB

#else

withTestDB :: (IO MySQL.ConnectInfo -> TestTree) -> TestTree
withTestDB = Tasty.withResource setup teardown
  where
    setup = do
        user <- fromMaybe "test" <$> lookupEnv "MYSQL_USER"
        password <- fromMaybe "test" <$> lookupEnv "MYSQL_PASSWORD"
        database <- fromMaybe "test" <$> lookupEnv "MYSQL_DATABASE"
        host <- fromMaybe "127.0.0.1" <$> lookupEnv "MYSQL_HOST"
        port <- maybe 3306 read <$> lookupEnv "MYSQL_PORT"

        let connectInfo =
                MySQL.defaultConnectInfoMB4
                    { ciUser = S8.pack user
                    , ciPassword = S8.pack password
                    , ciDatabase = S8.pack database
                    , ciHost = host
                    , ciPort = port
                    }

        bracket (MySQL.connect connectInfo) MySQL.close $ \conn -> do
            void $ MySQL.execute_ conn (fromString $ "DROP DATABASE IF EXISTS " ++ database)
            void $ MySQL.execute_ conn (fromString $ "CREATE DATABASE " ++ database)

        return connectInfo

    teardown = const $ return ()

#endif
