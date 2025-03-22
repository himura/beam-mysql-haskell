module Database.Beam.MySQL.Syntax.Spec where

import Database.Beam.MySQL.Syntax.SelectTableSpec qualified
import Test.Tasty

tests :: TestTree
tests =
    testGroup
        "Syntax"
        [ Database.Beam.MySQL.Syntax.SelectTableSpec.tests
        ]
