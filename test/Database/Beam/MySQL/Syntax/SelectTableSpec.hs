module Database.Beam.MySQL.Syntax.SelectTableSpec where

import Data.ByteString.Lazy qualified as L
import Data.Int
import Database.Beam
import Database.Beam.MySQL
import Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.MySQL.Test.Schema
import Database.Beam.MySQL.Test.Util
import Database.MySQL.Base
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "SelectTable"
        [ testGroup
            "IsSql92SelectTableSyntax"
            [ testGroup
                "selectTableStmt"
                [ testCase "select simple" $ do
                    let q1 = select $ do
                            user <- all_ blogDb.tableUser
                            guard_ $ user.name ==. val_ "Test User"
                            return user
                    assertSqlSelect q1 "SELECT `t0`.`id` AS `res0`,`t0`.`name` AS `res1` FROM `user` AS `t0` WHERE (`t0`.`name`) = (?)" [MySQLText "Test User"]
                , testCase "select group by" $ do
                    let q1 =
                            select $
                                aggregate_ (\post -> (group_ post.authorId, as_ @Int32 countAll_)) $
                                    all_ blogDb.tablePost
                    assertSqlSelect q1 "SELECT `t0`.`author_id` AS `res0`,COUNT(*) AS `res1` FROM `post` AS `t0` GROUP BY `t0`.`author_id`" []

                    let q2 =
                            select
                                $ aggregate_
                                    ( \comment ->
                                        let UserId authorId = comment.authorId
                                         in (group_ comment.postId, as_ @Int32 $ countOver_ distinctInGroup_ authorId, as_ @Int32 countAll_)
                                    )
                                $ all_ blogDb.tableComment
                    assertSqlSelect q2 "SELECT `t0`.`post_id` AS `res0`,COUNT(DISTINCT `t0`.`author_id`) AS `res1`,COUNT(*) AS `res2` FROM `comment` AS `t0` GROUP BY `t0`.`post_id`" []

                    let q3 =
                            select $
                                aggregate_ (\comment -> (group_ comment.postId, as_ @Int32 countAll_)) $ do
                                    comment <- all_ blogDb.tableComment
                                    guard_ $ comment.authorId ==. val_ (UserId 2)
                                    return comment
                    assertSqlSelect q3 "SELECT `t0`.`post_id` AS `res0`,COUNT(*) AS `res1` FROM `comment` AS `t0` WHERE (`t0`.`author_id`) = (?) GROUP BY `t0`.`post_id`" [MySQLInt32 2]
                , testCase "select group by having" $ do
                    let q1 =
                            select $
                                filter_ (\(_, n) -> n >=. val_ 5) $
                                    aggregate_ (\comment -> (group_ comment.postId, as_ @Int32 countAll_)) $
                                        all_ blogDb.tableComment
                    assertSqlSelect q1 "SELECT `t0`.`post_id` AS `res0`,COUNT(*) AS `res1` FROM `comment` AS `t0` GROUP BY `t0`.`post_id` HAVING (COUNT(*)) >= (?)" [MySQLInt32 5]
                ]
            , testGroup
                "unionTables"
                [ testCase "union" $ do
                    let q1 = do
                            post <- all_ blogDb.tablePost
                            return post.authorId
                        q2 = do
                            comment <- all_ blogDb.tableComment
                            return comment.authorId
                        q = select $ q1 `union_` q2
                        qAll = select $ q1 `unionAll_` q2

                    assertSqlSelect q "(SELECT `t0`.`author_id` AS `res0` FROM `post` AS `t0`) UNION (SELECT `t0`.`author_id` AS `res0` FROM `comment` AS `t0`)" []
                    assertSqlSelect qAll "(SELECT `t0`.`author_id` AS `res0` FROM `post` AS `t0`) UNION ALL (SELECT `t0`.`author_id` AS `res0` FROM `comment` AS `t0`)" []
                ]
            ]
        , testGroup
            "IsSql92SelectSyntax"
            [ testCase "ordering" $ do
                let q1 =
                        select $
                            orderBy_ (\post -> asc_ post.createdAt) $
                                all_ blogDb.tablePost
                assertSqlSelect q1 "SELECT `t0`.`id` AS `res0`,`t0`.`author_id` AS `res1`,`t0`.`title` AS `res2`,`t0`.`body` AS `res3`,`t0`.`created_at` AS `res4` FROM `post` AS `t0` ORDER BY `t0`.`created_at` ASC" []

                let q2 =
                        select $
                            orderBy_ (\(_, created, n) -> (desc_ n, asc_ created)) $
                                aggregate_ (\comment -> (group_ comment.postId, group_ comment.createdAt, as_ @Int32 countAll_)) $
                                    all_ blogDb.tableComment

                assertSqlSelect q2 "SELECT `t0`.`post_id` AS `res0`,`t0`.`created_at` AS `res1`,COUNT(*) AS `res2` FROM `comment` AS `t0` GROUP BY `t0`.`post_id`,`t0`.`created_at` ORDER BY COUNT(*) DESC,`t0`.`created_at` ASC" []
            , testCase "limit, offset" $ do
                let q1 = select $ limit_ 10 $ all_ blogDb.tableUser
                assertSqlSelect q1 "SELECT `t0`.`id` AS `res0`,`t0`.`name` AS `res1` FROM `user` AS `t0` LIMIT 10" []

                let q2 = select $ limit_ 10 $ offset_ 120 $ all_ blogDb.tableUser
                assertSqlSelect q2 "SELECT `t0`.`id` AS `res0`,`t0`.`name` AS `res1` FROM `user` AS `t0` LIMIT 10 OFFSET 120" []
            ]
        ]

assertSqlSelect :: SqlSelect MySQL a -> L.ByteString -> [MySQLValue] -> Assertion
assertSqlSelect (SqlSelect q) = assertMySQLSyntax (fromMySQLSelect q)
