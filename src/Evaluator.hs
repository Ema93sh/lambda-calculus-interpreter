module Evaluator where

import Syntax
import Data.Foldable
import Data.Function (on)
import Data.Maybe
import Data.List
import Trace
import Pretty
import qualified Debug.Trace


alphaEquivalence :: LExpr -> LExpr -> Bool
alphaEquivalence (Var x) (Var y)         =  x == y
alphaEquivalence (App e1 e2) (App e3 e4) =  alphaEquivalence e1 e3 &&  alphaEquivalence e2 e4
alphaEquivalence (Abs x e1) (Abs y e2)   =  alphaEquivalence (substitute x (Var y) e1) e2
alphaEquivalence _ _                     =  False


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


-- Substitute without capture avoiding
substitute' :: Name -> LExpr -> LExpr -> LExpr
substitute' x y expr@(Var z)
                              | z == x         = y
                              | z /= x         = expr

substitute' x y (App e1 e2)                     = App (substitute' x y e1) (substitute' x y e2)
substitute' x y expr@(Abs l e)
      | x == l                                 = expr
      | otherwise                              = Abs l (substitute' x y e)


betaReduction :: (Name -> LExpr -> LExpr -> LExpr) -> LExpr -> LExpr
betaReduction substitute e@(App (Abs x expr) y) = let e' =  substitute x y expr in
                                                  if alphaEquivalence e e' then e else betaReduction substitute e'
betaReduction   _           expr                = expr


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


redexes :: LExpr -> [LExpr]
redexes (Var _)        = []
redexes (Abs x expr)   = redexes expr
redexes (App l r)      = case canApply l r of
                            Just e  -> [e] ++ redexes l ++ redexes r
                            Nothing -> redexes l ++ redexes r

isNormal :: LExpr -> Bool
isNormal = null . redexes

replace :: LExpr -> LExpr -> LExpr -> LExpr
replace expr         x y
      | expr == x           =  y
replace expr@(Var _) x y    = expr
replace expr@(App l r) x y  = let l' = replace l x y in --
                                if l' /= l then (App l' r)
                                           else (App l (replace r x y))

replace expr@(Abs a e) x y  = Abs a (replace e x y)

betaNormal :: LExpr -> LExpr
betaNormal expr = case normalOrderRedex expr of
              Just r -> let e' = replace expr r (betaReduction substitute r) in
                        if e' == expr then expr else betaNormal e'
              Nothing -> expr

eval :: LExpr -> (LExpr -> Maybe LExpr) -> Trace LExpr
eval expr redex = case redex expr of
                      Just r -> do
                                  trace expr r
                                  let e' = replace expr r (betaReduction substitute r)
                                  if alphaEquivalence e' expr then return expr else eval e' redex
                      Nothing -> return expr

genOneLevel :: String -> LExpr -> LExpr -> (LExpr -> Maybe LExpr) -> [(String, LExpr)]
genOneLevel trace expr normalForm strategy = catMaybes [ reduce r s | s <- [True, False], r <- catMaybes [strategy expr]]
                                    where
                                      reduce r s = let e' = replace expr r (betaReduction (chooseFn s) r) in
                                                  if alphaEquivalence e' expr then Nothing else Just ((trace ++ "\n" ++ "→" ++ prettyReduce e' r s), e')
                                      chooseFn t = if t then substitute else substitute'


genTree' :: String -> LExpr -> LExpr ->  (LExpr -> Maybe LExpr) -> LTree
genTree' trace expr normalForm strategy
      | isNormal expr = Node (trace, expr) []
      | otherwise     = Node (trace, expr) $ map (\(t, e) -> genTree' t e normalForm strategy) (genOneLevel trace expr normalForm strategy)


genTree expr normalForm strategy = genTree' (show expr) expr normalForm strategy


exprs :: LTree -> [(String, LExpr)]
exprs l@(Node (t,e) []) = [(t,e)]
exprs  (Node _ xs)  = concatMap exprs xs

findExpr :: LTree -> LExpr -> String
findExpr tree expr = maybe "¯\\_(ツ)_/¯" fst (find (\(t, e) ->  alphaEquivalence e expr ) (exprs tree))

explain :: LExpr -> LExpr -> (LExpr -> Maybe LExpr) -> String
explain e1 e2 strategy
  | e2 /= betaNormal e1 = findExpr (genTree e1 (betaNormal e1) strategy) e2
  | otherwise            = show $ eval e1 strategy
