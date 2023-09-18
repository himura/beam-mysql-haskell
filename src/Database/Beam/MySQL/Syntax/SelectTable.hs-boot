module Database.Beam.MySQL.Syntax.SelectTable
    ( MySQLSelectTableSyntax (..)
    , MySQLSelectSyntax (..)
    ) where

import Database.Beam.MySQL.Syntax.Type (MySQLSyntax)

newtype MySQLSelectTableSyntax = MySQLSelectTableSyntax {fromMySQLSelectTable :: MySQLSyntax}

-- | MySQL @SELECT@ Syntax
newtype MySQLSelectSyntax = MySQLSelectSyntax {fromMySQLSelect :: MySQLSyntax}
