module Syntax where

import Data.List
import Data.Maybe

type Name = Char

data LExpr =  Abs Name LExpr
            | App LExpr LExpr
            | Var Name

data DExpr =  DVal Int
            | DApp DExpr DExpr
            | DAbs DExpr


data LTree =  Node (String, LExpr) [LTree]



debruijn :: LExpr -> DExpr
debruijn e = debruijn' [] e

debruijn' :: [Name] -> LExpr -> DExpr
debruijn' context (Var v)     = DVal $ fromMaybe (length context) (elemIndex v context)
debruijn' context (Abs v e)   = DAbs (debruijn' (v : context) e)
debruijn' context (App e1 e2) = DApp (debruijn' context e1) (debruijn' context e2)


-- (\x.xx)y
example1 :: LExpr
example1 = App (Abs 'x' (App (Var 'x') (Var 'x'))) (Var 'y')

-- (\x.xx)((\xy.yx)y)
example2 :: LExpr
example2 = App (Abs 'x' (App (Var 'x') (Var 'x'))) (App (Abs 'x' (Abs 'y' (App (Var 'y') (Var 'x')))) (Var 'y'))

-- (\x.xx)
example3 :: LExpr
example3 = Abs 'x' (App (Var 'x') (Var 'x'))

-- (λx.(λy.xy))y
example4 :: LExpr
example4 = App (Abs 'x' (Abs 'y' (App (Var 'x') (Var 'y')))) (Var 'y')

-- (λx.xx)((λxy.yx)y)
example5 :: LExpr
example5 = App (example3) (App (Abs 'x' (Abs 'y' (App (Var 'y') (Var 'x')))) (Var 'y'))

-- (\a.ya)
alphaExample1 :: LExpr
alphaExample1 = Abs 'a' (App (Var 'y') (Var 'a'))

alphaExample2 :: LExpr
alphaExample2 = Abs 'x' (App (Var 'z') (Var 'x'))

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
                         head $ filter (\x -> x `notElem` vars)  ['a'..'z']

instance Eq DExpr where
  (DVal x)   == (DVal y)    = x == y
  (DApp x y) == (DApp a b)  = (x == a) && (y == b)
  (DAbs x)   == (DAbs y)    = (x == y)
  _          ==  _          = False

instance Eq LExpr where
  (Var x)   == (Var y)    = x == y
  (App x y) == (App a b)  = (x == a) && (y == b)
  (Abs x y) == (Abs z k)  = (x == z) && (y == k)
  _         ==   _        = False
