module Syntax where

import Data.List

type Name = Char

data LExpr =  Abs Name LExpr
            | App LExpr LExpr
            | Var Name


args :: LExpr -> [Name]
args (Abs x e)  = x : args e
args  otherwise = []

body :: LExpr -> LExpr
body (Abs x e) = body e
body  e        = e

freeVariables :: LExpr -> [Name]
freeVariables (Var z)         = [z]
freeVariables (App e1 e2)     = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Abs x  e)      = nub $ filter (/= x) (freeVariables e)

boundVariables :: LExpr -> [Name]
boundVariables (Var z)        = []
boundVariables (App e1 e2)    = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Abs x e)      = nub $ [x] ++ boundVariables e

freshVariable :: LExpr -> Name
freshVariable expr = let vars = freeVariables expr ++ boundVariables expr
                      in
                         head $ filter(\x -> x `notElem` vars) ['a'..'z']


instance Eq LExpr where
  (Var x)   == (Var y)     = x == y
  (App x y) == (App a b)   = (x == a) && (y == b)
  (Abs x e) == (Abs y t)   = (x == y) && (e == t)
  _         == _           = False
