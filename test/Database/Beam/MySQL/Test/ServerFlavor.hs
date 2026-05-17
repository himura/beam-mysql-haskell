-- | Tasty option for selecting which server flavor (MariaDB or MySQL) the
-- integration tests should run against. Set via @--server-flavor=mariadb@ on
-- the command line or @TASTY_SERVER_FLAVOR=mariadb@ in the environment.
-- Default is 'MariaDB' so that vendor-specific tests fail loudly when run
-- against the wrong server, rather than silently being skipped.
module Database.Beam.MySQL.Test.ServerFlavor
    ( ServerFlavor (..)
    , serverFlavorOption
    ) where

import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged)
import Test.Tasty.Options (IsOption (..), OptionDescription (..))

data ServerFlavor
    = MariaDBServer
    | MySQLServer
    deriving stock (Show, Eq)

instance IsOption ServerFlavor where
    defaultValue = MariaDBServer
    parseValue = \case
        "mariadb" -> Just MariaDBServer
        "mysql" -> Just MySQLServer
        _ -> Nothing
    optionName = pure "server-flavor" :: Tagged ServerFlavor String
    optionHelp =
        pure "Server flavor under test (mariadb, mysql). Default: mariadb."
            :: Tagged ServerFlavor String

serverFlavorOption :: OptionDescription
serverFlavorOption = Option (Proxy :: Proxy ServerFlavor)
