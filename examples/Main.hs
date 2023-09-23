module Main where

import Data.Pool
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Database.Beam
import Database.Beam.MySQL.Connection
import Database.MySQL.Base as MySQL
import StudentDB.Enum
import StudentDB.Schema

newMySQLPool :: MySQL.ConnectInfo -> IO (Pool MySQL.MySQLConn)
newMySQLPool connectInfo = newPool $ setNumStripes (Just 5) $ defaultPoolConfig (MySQL.connect connectInfo) MySQL.close 10 5

runDB :: Pool MySQL.MySQLConn -> MySQLM a -> IO a
runDB pool act = do
    withResource pool $ \conn -> MySQL.withTransaction conn $ runBeamMySQLMWithDebug T.putStrLn conn act

main :: IO ()
main = do
    let connectInfo = defaultConnectInfoMB4{ciDatabase = "beamtestdb", ciUser = "test", ciPassword = "test"}
    connPool <- newMySQLPool connectInfo

    runDB connPool $ do
        runDelete $ delete userDirectoryDb.tableUserGroup (\_ -> val_ True)
        runDelete $ delete userDirectoryDb.tableUser (\_ -> val_ True)
        runDelete $ delete userDirectoryDb.tableGroup (\_ -> val_ True)
        runDelete $ delete userDirectoryDb.tableSchool (\_ -> val_ True)

    let releasedAt = LocalTime (fromGregorian 2021 2 4) midday

    runDB connPool $ do
        runInsert $
            insert userDirectoryDb.tableSchool $
                insertValues
                    [ School 1 "アビドス高等学校" releasedAt releasedAt
                    , School 2 "ゲヘナ学園" releasedAt releasedAt
                    , School 3 "トリニティ総合学園" releasedAt releasedAt
                    ]
        runInsert $
            insert userDirectoryDb.tableGroup $
                insertValues
                    [ Group 1 "対策委員会" releasedAt releasedAt
                    , Group 2 "覆面水着団" releasedAt releasedAt
                    , Group 3 "万魔殿" releasedAt releasedAt
                    , Group 4 "ティーパーティー" releasedAt releasedAt
                    ]

        runInsert $
            insert userDirectoryDb.tableUser $
                insertExpressions
                    [ User default_ (val_ "yume@abydos.ac.kv") (val_ UserInactive) (val_ "ユメ") (val_ $ SchoolId 1) default_ default_
                    , User default_ (val_ "hoshino@abydos.ac.kv") (val_ UserActive) (val_ "小鳥遊ホシノ") (val_ $ SchoolId 1) default_ default_
                    , User default_ (val_ "nonomi@abydos.ac.kv") (val_ UserActive) (val_ "ノノミ") (val_ $ SchoolId 1) default_ default_
                    , User default_ (val_ "shirokokawaii@abydos.ac.kv") (val_ UserActive) (val_ "シロコ") (val_ $ SchoolId 1) default_ default_
                    , User default_ (val_ "azunyan@abydos.ac.kv") (val_ UserActive) (val_ "セリカ") (val_ $ SchoolId 1) default_ default_
                    , User default_ (val_ "natsume-iroha@ps.gehenna.ac.kv") (val_ UserActive) (val_ "棗イロハ") (val_ $ SchoolId 2) default_ default_
                    , User default_ (val_ "hifumidaisuki@trinity.ac.kv") (val_ UserActive) (val_ "阿慈谷ヒフミ") (val_ $ SchoolId 3) default_ default_
                    , User default_ (val_ "yurizono.seia@trinity.ac.kv") (val_ UserNotImplemented) (val_ "百合園セイア") (val_ $ SchoolId 3) default_ default_
                    ]

        -- 対策委員会
        runInsert $
            insertOnly userDirectoryDb.tableUserGroup (\ug -> (ug.userId, ug.groupId)) $
                insertFrom $ do
                    user <- all_ userDirectoryDb.tableUser
                    guard_ $ user.schoolId ==. val_ (SchoolId 1) &&. user.state ==. val_ UserActive
                    return $ (pk user, GroupId (val_ 1) :: PrimaryKey GroupT (QExpr MySQL QBaseScope))

        -- 覆面水着団
        runInsert $
            insertOnly userDirectoryDb.tableUserGroup (\ug -> (ug.userId, ug.groupId)) $
                insertFrom $ do
                    user <- all_ userDirectoryDb.tableUser
                    guard_ $ (user.schoolId ==. val_ (SchoolId 1) &&. user.state ==. val_ UserActive) ||. (user.email ==. val_ "hifumidaisuki@trinity.ac.kv")
                    return $ (pk user, GroupId (val_ 2) :: PrimaryKey GroupT (QExpr MySQL QBaseScope))

    putStrLn "Print all results"

    results <- runDB connPool $
        runSelectReturningList $
            select $ do
                user <- all_ userDirectoryDb.tableUser
                school <- related_ userDirectoryDb.tableSchool user.schoolId
                return (user, school)
    mapM_ (T.putStrLn . showResult) results

    resultsFilt <- runDB connPool $
        runSelectReturningList $
            select $ do
                user <- all_ userDirectoryDb.tableUser
                targetUserGroup <- nub_ $ do
                    ug <- all_ userDirectoryDb.tableUserGroup
                    guard_ $ ug.groupId ==. val_ (GroupId 2)
                    return ug.userId
                guard_ $ targetUserGroup `references_` user
                school <- related_ userDirectoryDb.tableSchool user.schoolId
                return (user, school)
    mapM_ (T.putStrLn . showResult) resultsFilt

showResult :: (User, School) -> T.Text
showResult (user, school) =
    T.intercalate
        "\t"
        [ T.pack $ show user.id
        , user.email
        , user.name
        , T.pack $ show user.state
        , T.pack $ show user.createdAt
        , T.pack $ show user.updatedAt
        , T.pack $ show school.id
        , school.name
        , T.pack $ show school.createdAt
        , T.pack $ show school.updatedAt
        ]
