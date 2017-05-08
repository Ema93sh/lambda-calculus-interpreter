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


canApply :: LExpr -> LExpr -> Maybe LExpr
canApply l@(Abs _ _) r = Just (App l r)
canApply otherwise   _ = Nothing



-- Normal Order :- Leftmost
normalOrderRedex :: LExpr -> Maybe LExpr
normalOrderRedex (Var _)       = Nothing
normalOrderRedex (Abs x expr)  = normalOrderRedex expr
normalOrderRedex (App l r)     = msum [canApply l r, normalOrderRedex l, normalOrderRedex r]


--  Applicative Order :- Innermost Leftmost
applicativeOrderRedex :: LExpr -> Maybe LExpr
applicativeOrderRedex (Var _)      = Nothing
applicativeOrderRedex (Abs x expr) = applicativeOrderRedex expr
applicativeOrderRedex (App l r)    = msum [applicativeOrderRedex l, applicativeOrderRedex r, canApply l r]


replace :: LExpr -> LExpr -> LExpr -> LExpr
replace expr         x y
      | expr == x           =  y
replace expr@(Var _) x y    = expr
replace expr@(App l r) x y  = let l' =  replace l x y in
                                if l' /= l then (App l' r)
                                           else (App l (replace r x y))

replace expr@(Abs a e) x y  = Abs a (replace e x y)


eval :: LExpr -> (LExpr -> Maybe LExpr) -> Trace LExpr
eval expr redex = case redex expr of
              Just r -> let t = (betaReduction r) in
                          do
                            trace expr r
                            eval (replace expr r t) redex
              Nothing -> return expr
