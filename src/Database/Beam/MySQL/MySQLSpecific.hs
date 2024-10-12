{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database.Beam.MySQL.MySQLSpecific (rlike_) where

import Database.Beam.Backend.SQL (BeamSqlBackendIsString)
import Database.Beam.MySQL.Backend (MySQL)
import Database.Beam.MySQL.Syntax.Expression
    ( MySQLExpressionSyntax
        ( MySQLExpressionSyntax
        , fromMySQLExpression
        )
    )
import Database.Beam.MySQL.Syntax.Type (emit)
import Database.Beam.Query (QGenExpr (..))

rlike_
    :: (BeamSqlBackendIsString MySQL text)
    => QGenExpr ctxt MySQL s text
    -- ^ String to search
    -> QGenExpr ctxt MySQL s text
    -- ^ Regular expression to search for
    -> QGenExpr ctxt MySQL s Bool
rlike_ (QExpr s) (QExpr re) =
    QExpr $ \t -> MySQLExpressionSyntax $ fromMySQLExpression (s t) <> emit " RLIKE " <> fromMySQLExpression (re t)
