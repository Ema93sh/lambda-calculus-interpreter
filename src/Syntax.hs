module Syntax where


type Name = Char

data LExpr =  Abs Name LExpr
            | App LExpr LExpr
            | Var Name
            deriving (Show)
