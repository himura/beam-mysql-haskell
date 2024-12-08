{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database.Beam.MySQL.MySQLSpecific (rlike_, lastInsertId_) where

import Data.Word (Word64)
import Database.Beam.Backend.SQL (BeamSqlBackendIsString)
import Database.Beam.MySQL.Backend (MySQL)
import Database.Beam.MySQL.Syntax.Expression
    ( MySQLExpressionSyntax
        ( MySQLExpressionSyntax
        , fromMySQLExpression
        )
    )
import Database.Beam.MySQL.Syntax.Type (emit)
import Database.Beam.Query (QExpr, QGenExpr (..))

rlike_
    :: (BeamSqlBackendIsString MySQL text)
    => QGenExpr ctxt MySQL s text
    -- ^ String to search
    -> QGenExpr ctxt MySQL s text
    -- ^ Regular expression to search for
    -> QGenExpr ctxt MySQL s Bool
rlike_ (QExpr s) (QExpr re) =
    QExpr $ \t -> MySQLExpressionSyntax $ fromMySQLExpression (s t) <> emit " RLIKE " <> fromMySQLExpression (re t)

lastInsertId_ :: QExpr MySQL s Word64
lastInsertId_ =
    QExpr (\_ -> MySQLExpressionSyntax $ emit "last_insert_id()")
