module Evaluator where

import Syntax
import Control.Monad.Identity

-- [y/x] (\t.xt)
substitute :: Name -> LExpr -> LExpr -> LExpr
substitute x y expr@(Var z)    = if z == x then y else expr
substitute x y (App e1 e2)     = App (substitute x y e1) (substitute x y e2)
substitute x y expr@(Abs l e)
              | x == l         = expr
              | otherwise      = Abs l (substitute x y e)


betaReduction :: LExpr -> LExpr
betaReduction (App (Abs x expr) y) = betaReduction $ substitute x y expr
betaReduction       e              = e
