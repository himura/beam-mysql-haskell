-- | MySQL/MariaDB-specific query features.
--
-- == Locking reads
--
-- 'lockingFor_' and its variants add a locking clause such as @FOR UPDATE@ to
-- a @SELECT@ statement. Locking reads take effect only inside a transaction.
--
-- The locking clause is rendered exactly as specified. This library does not
-- detect the server flavor and does not rewrite the clause into a form that
-- the server accepts. MySQL and MariaDB accept different syntax, so choose one
-- that your server accepts:
--
-- * Accepted by both MySQL 8.0.1+ and MariaDB 10.6+: @FOR UPDATE@,
--   @FOR UPDATE NOWAIT@, @FOR UPDATE SKIP LOCKED@ and @LOCK IN SHARE MODE@.
-- * MySQL only: @FOR SHARE@, with or without @NOWAIT@ / @SKIP LOCKED@.
-- * MariaDB only: @WAIT n@, and @LOCK IN SHARE MODE@ with @NOWAIT@,
--   @SKIP LOCKED@ or @WAIT n@.
--
-- @OF tbl_name@ and multiple locking clauses are not supported.
--
-- See
-- <https://github.com/himura/beam-mysql-haskell/blob/main/docs/locking-reads.md docs/locking-reads.md>
-- for details, including version compatibility and sources.
module Database.Beam.MySQL.Full
    ( -- * Locking reads
      lockingFor_
    , lockingForShare_
    , lockingForUpdate_
    , lockingInShareMode_
    , MySQLSelectLockingStrength (..)
    , MySQLSelectLockingOption (..)
    ) where

import Control.Monad.Free.Church
import Data.Proxy
import Database.Beam.MySQL.Backend
import Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.Query
import Database.Beam.Query.Internal

-- | Lock the rows read by the query with the given strength and option.
--
-- For example,
--
-- > lockingFor_ MySQLSelectLockingStrengthUpdate (Just MySQLSelectLockingOptionSkipLocked)
--
-- renders @FOR UPDATE SKIP LOCKED@.
lockingFor_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => MySQLSelectLockingStrength
    -> Maybe MySQLSelectLockingOption
    -> Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingFor_ lockStrength lockOption (Q q) =
    Q
        ( liftF
            ( QForceSelect
                ( \(_ :: a) ->
                    mysqlSelectStmt (Just (MySQLSelectLockingSyntax lockStrength lockOption))
                )
                q
                (rewriteThread (Proxy @s))
            )
        )

-- | @FOR SHARE@. MySQL 8.0.1+ only; MariaDB does not support it. Use
-- 'lockingInShareMode_' for MariaDB.
lockingForShare_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingForShare_ = lockingFor_ MySQLSelectLockingStrengthShare Nothing

-- | @FOR UPDATE@.
lockingForUpdate_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingForUpdate_ = lockingFor_ MySQLSelectLockingStrengthUpdate Nothing

-- | @LOCK IN SHARE MODE@. Equivalent to @FOR SHARE@, and accepted by both
-- MySQL and MariaDB.
lockingInShareMode_
    :: forall db s a
     . (Projectible MySQL a, ThreadRewritable (QNested s) a)
    => Q MySQL db (QNested s) a
    -> Q MySQL db s (WithRewrittenThread (QNested s) s a)
lockingInShareMode_ = lockingFor_ MySQLSelectLockingStrengthLockInShareMode Nothing
