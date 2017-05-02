module Evaluator where

import Syntax
import Data.Foldable
import Trace

-- [y/x] (\t.xt)
substitute :: Name -> LExpr -> LExpr -> LExpr
substitute x y expr@(Var z)
                              | z == x         = y
                              | z /= x         = expr

substitute x y (App e1 e2)                     = App (substitute x y e1) (substitute x y e2)
substitute x y expr@(Abs l e)
      | x == l                                 = expr
      | x /= l  && l `elem` freeVariables y    = let v = freshVariable expr in
                                                  Abs v (substitute x y (substitute l (Var v) e))
      | otherwise                              = Abs l (substitute x y e)



betaReduction :: LExpr -> LExpr
betaReduction e@(App (Abs x expr) y) = betaReduction $ substitute x y expr
betaReduction    expr                = expr


-- Normal Order :- Leftmost innermost
redex :: LExpr -> Maybe LExpr
redex expr@(Var _)      = Nothing
redex (Abs x expr)      = redex expr
redex expr@(App l r)    = msum [redex l, redex r, canApply l]
                          where
                            canApply e@(Abs _ _) = Just expr
                            canApply otherwise   = Nothing

replace :: LExpr -> LExpr -> LExpr -> LExpr
replace expr         x y
      | expr == x           =  y
replace expr@(Var _) x y    = expr
replace expr@(App l r) x y  = App (replace l x y) (replace r x y)
replace expr@(Abs a e) x y  = Abs a (replace e x y)


eval :: LExpr -> Trace LExpr
eval expr  = case redex expr of
              Just r -> let t = (betaReduction r) in
                          do
                            trace expr r
                            eval $ replace expr r t
              Nothing -> return expr
