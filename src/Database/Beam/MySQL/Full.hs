module Database.Beam.MySQL.Full
    ( lockingFor_
    , lockingForShare_
    , lockingForUpdate_
    ) where

import Control.Monad.Free.Church
import Data.Proxy
import Database.Beam.MySQL.Backend
import Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.Query
import Database.Beam.Query.Internal

lockingFor_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => MySQLSelectLockingStrength
    -> Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingFor_ lockStrength (Q q) =
    Q
        ( liftF
            ( QForceSelect
                ( \(_ :: a) ->
                    mysqlSelectStmt (Just (MySQLSelectLockingSyntax lockStrength))
                )
                q
                (rewriteThread (Proxy @s))
            )
        )

lockingForShare_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingForShare_ = lockingFor_ MySQLSelectLockingStrengthShare

lockingForUpdate_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingForUpdate_ = lockingFor_ MySQLSelectLockingStrengthUpdate
