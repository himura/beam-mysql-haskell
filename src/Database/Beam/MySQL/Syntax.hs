module Database.Beam.MySQL.Syntax (MySQLCommandSyntax (..)) where

import Data.Coerce
import Database.Beam.Backend.SQL
import Database.Beam.MySQL.Syntax.Delete
import Database.Beam.MySQL.Syntax.Insert
import Database.Beam.MySQL.Syntax.SelectTable
import Database.Beam.MySQL.Syntax.Type
import Database.Beam.MySQL.Syntax.Update

newtype MySQLCommandSyntax
    = MySQLCommandQuery MySQLSyntax
    deriving stock (Show)

instance IsSql92Syntax MySQLCommandSyntax where
    type Sql92SelectSyntax MySQLCommandSyntax = MySQLSelectSyntax
    type Sql92InsertSyntax MySQLCommandSyntax = MySQLInsertSyntax
    type Sql92UpdateSyntax MySQLCommandSyntax = MySQLUpdateSyntax
    type Sql92DeleteSyntax MySQLCommandSyntax = MySQLDeleteSyntax

    -- todo
    selectCmd :: MySQLSelectSyntax -> MySQLCommandSyntax
    selectCmd = coerce
    insertCmd :: MySQLInsertSyntax -> MySQLCommandSyntax
    insertCmd = coerce
    updateCmd :: MySQLUpdateSyntax -> MySQLCommandSyntax
    updateCmd = coerce
    deleteCmd :: MySQLDeleteSyntax -> MySQLCommandSyntax
    deleteCmd = coerce
