module Database.Beam.MySQL.Test.Util where

import Data.ByteString.Lazy qualified as L
import Data.DList qualified as DList
import Database.Beam.MySQL.Syntax.Type
    ( MySQLSyntax (MySQLSyntax)
    , buildSqlWithPlaceholder
    )
import Database.MySQL.Base (MySQLValue)
import GHC.Stack
import Test.Tasty.HUnit (Assertion, (@?=))

assertMySQLSyntax :: MySQLSyntax -> L.ByteString -> [MySQLValue] -> Assertion
assertMySQLSyntax (MySQLSyntax sql params) expectedSql expectedParams = withFrozenCallStack $ do
    buildSqlWithPlaceholder sql @?= expectedSql
    DList.toList params @?= expectedParams
