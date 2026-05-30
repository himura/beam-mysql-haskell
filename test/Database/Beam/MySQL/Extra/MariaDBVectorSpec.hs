{-# LANGUAGE NoFieldSelectors #-}

module Database.Beam.MySQL.Extra.MariaDBVectorSpec where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Either (isLeft)
import Data.Text (Text)
import Database.Beam
import Database.Beam.MySQL
import Database.Beam.MySQL.Extra.MariaDBVector
import Database.Beam.MySQL.Extra.Schema
import Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.MySQL.Test.Util
import Database.MySQL.Base
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "MariaDBVector"
        [ conversionTests
        , syntaxTests
        ]

conversionTests :: TestTree
conversionTests =
    testGroup
        "fromFloats / toFloats"
        [ testCase "round-trip empty list" $
            toFloats (fromFloats []) @?= Right []
        , testCase "round-trip single element" $
            toFloats (fromFloats [1.5]) @?= Right [1.5]
        , testCase "round-trip multiple elements" $
            toFloats (fromFloats [0.1, -2.5, 3.14, 0.0]) @?= Right [0.1, -2.5, 3.14, 0.0]
        , testCase "fromFloats [] yields empty bytes" $
            unMariaDBVector (fromFloats []) @?= BS.empty
        , testCase "fromFloats [1.0] is IEEE 754 LE" $
            -- 1.0 in IEEE 754 single precision = 0x3F800000, little-endian on disk
            unMariaDBVector (fromFloats [1.0]) @?= BS.pack [0x00, 0x00, 0x80, 0x3F]
        , testCase "fromFloats [-1.0] is IEEE 754 LE" $
            unMariaDBVector (fromFloats [-1.0]) @?= BS.pack [0x00, 0x00, 0x80, 0xBF]
        , testCase "fromFloats [0.0] is all zeros" $
            unMariaDBVector (fromFloats [0.0]) @?= BS.pack [0x00, 0x00, 0x00, 0x00]
        , testCase "fromFloats packs floats in order" $
            unMariaDBVector (fromFloats [1.0, 2.0])
                @?= BS.pack [0x00, 0x00, 0x80, 0x3F, 0x00, 0x00, 0x00, 0x40]
        , testCase "toFloats parses empty bytes as empty list" $
            toFloats (MariaDBVector BS.empty) @?= Right []
        , testCase "toFloats rejects 1-byte input" $
            isLeft (toFloats (MariaDBVector (BS.pack [0x00]))) @? "expected Left"
        , testCase "toFloats rejects 5-byte input (not multiple of 4)" $
            isLeft (toFloats (MariaDBVector (BS.pack [0x00, 0x00, 0x80, 0x3F, 0x00])))
                @? "expected Left"
        ]

syntaxTests :: TestTree
syntaxTests =
    testGroup
        "SQL syntax"
        [ testCase "vecDistance_ emits VEC_DISTANCE" $ do
            let q =
                    select $ do
                        e <- all_ embedDb.tableEmbed
                        return (e.id, vecDistance_ e.vec (val_ (fromFloats [1.0, 2.0])))
            assertSqlSelect
                q
                "SELECT `t0`.`id` AS `res0`,VEC_DISTANCE(`t0`.`vec`,?) AS `res1` FROM `embed` AS `t0`"
                [MySQLBytes (unMariaDBVector (fromFloats [1.0, 2.0]))]
        , testCase "vecDistanceCosine_ emits VEC_DISTANCE_COSINE" $ do
            let q =
                    select $ do
                        e <- all_ embedDb.tableEmbed
                        return (e.id, vecDistanceCosine_ e.vec (val_ (fromFloats [1.0])))
            assertSqlSelect
                q
                "SELECT `t0`.`id` AS `res0`,VEC_DISTANCE_COSINE(`t0`.`vec`,?) AS `res1` FROM `embed` AS `t0`"
                [MySQLBytes (unMariaDBVector (fromFloats [1.0]))]
        , testCase "vecDistanceEuclidean_ emits VEC_DISTANCE_EUCLIDEAN" $ do
            let q =
                    select $ do
                        e <- all_ embedDb.tableEmbed
                        return (e.id, vecDistanceEuclidean_ e.vec (val_ (fromFloats [1.0])))
            assertSqlSelect
                q
                "SELECT `t0`.`id` AS `res0`,VEC_DISTANCE_EUCLIDEAN(`t0`.`vec`,?) AS `res1` FROM `embed` AS `t0`"
                [MySQLBytes (unMariaDBVector (fromFloats [1.0]))]
        , testCase "vecFromText_ emits VEC_FromText" $ do
            let q =
                    select $ do
                        e <- all_ embedDb.tableEmbed
                        return (e.id, vecDistance_ e.vec (vecFromText_ (val_ ("[1,2,3]" :: Text))))
            assertSqlSelect
                q
                "SELECT `t0`.`id` AS `res0`,VEC_DISTANCE(`t0`.`vec`,VEC_FromText(?)) AS `res1` FROM `embed` AS `t0`"
                [MySQLText "[1,2,3]"]
        , testCase "vecToText_ emits VEC_ToText" $ do
            let q =
                    select $ do
                        e <- all_ embedDb.tableEmbed
                        return (e.id, vecToText_ e.vec :: QGenExpr QValueContext MySQL QBaseScope Text)
            assertSqlSelect
                q
                "SELECT `t0`.`id` AS `res0`,VEC_ToText(`t0`.`vec`) AS `res1` FROM `embed` AS `t0`"
                []
        , testCase "vecDistance_ in ORDER BY for nearest-neighbour search" $ do
            let q =
                    select $
                        orderBy_ (\(_, d) -> asc_ d) $ do
                            e <- all_ embedDb.tableEmbed
                            return (e.id, vecDistance_ e.vec (val_ (fromFloats [0.0, 0.0])))
            assertSqlSelect
                q
                "SELECT `t0`.`id` AS `res0`,VEC_DISTANCE(`t0`.`vec`,?) AS `res1` FROM `embed` AS `t0` ORDER BY VEC_DISTANCE(`t0`.`vec`,?) ASC"
                [ MySQLBytes (unMariaDBVector (fromFloats [0.0, 0.0]))
                , MySQLBytes (unMariaDBVector (fromFloats [0.0, 0.0]))
                ]
        ]

assertSqlSelect :: SqlSelect MySQL a -> L.ByteString -> [MySQLValue] -> Assertion
assertSqlSelect (SqlSelect q) = assertMySQLSyntax (fromMySQLSelect q)
