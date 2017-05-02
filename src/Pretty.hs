{-# LANGUAGE UnicodeSyntax #-}

module Pretty (
      prettyPrint,
      prettyReduce
      )
    where

import Syntax
import Text.PrettyPrint.ANSI.Leijen

-- TODO: Fix add brackets for applications

class PrettyLambda a where
  pPrint :: Int -> a -> Doc
  pReduction :: Int -> a -> a -> Doc

instance PrettyLambda LExpr where
  pPrint   _    (Var x)        = char x
  pPrint depth expr@(App l r)  = printApp depth l r
  pPrint depth expr            = hcat [ text "\\",
                                        text $ args expr,
                                        text ".",
                                        pPrint (depth) $ body expr
                                      ]


  pReduction _   (Var x)  _          = char x
  pReduction d  expr redex
                  | expr == redex    = underline $ pPrint d expr
  pReduction d (App l r) redex       = rPrintApp d l r redex
  pReduction d expr@(Abs x e) redex  = hcat [  text "\\",
                                                text $ args expr,
                                                text ".",
                                                pReduction d (body expr) redex
                                              ]


-- TODO UGLY
printApp :: Int -> LExpr -> LExpr -> Doc
printApp depth x@(Var _)  y@(Var _) = pPrint (depth + 1) x <> pPrint (depth + 1) y
printApp depth  x         y@(Var _) = parens (pPrint (depth + 1) x) <> pPrint  (depth + 1) y
printApp depth x@(Var _)    y       = pPrint (depth + 1) x <+> parens (pPrint (depth + 1) y)
printApp depth x            y       = parens (pPrint (depth + 1) x) <> parens (pPrint (depth + 1) y)

rPrintApp :: Int -> LExpr -> LExpr -> LExpr -> Doc
rPrintApp d x@(Var _)  y@(Var _) r = pPrint (d + 1) x <> pPrint (d + 1) y
rPrintApp d  x         y@(Var _) r = parens (pReduction (d+1) x r) <> pPrint  (d + 1) y
rPrintApp d x@(Var _)    y       r = pPrint (d + 1) x <+> parens (pReduction (d + 1) y r)
rPrintApp d x            y       r = parens (pReduction (d+1) x r) <> parens (pReduction (d + 1) y r)


depthParens :: Int -> Doc -> Doc
depthParens 0  = id
depthParens _  = parens

prettyPrint :: LExpr -> String
prettyPrint e = displayS ( renderPretty 1 100 (pPrint 0 e) ) ""

prettyReduce :: LExpr -> LExpr -> String
prettyReduce e r = displayS ( renderPretty 1 100 (pReduction 0 e r) )""

instance Show LExpr where
  show = prettyPrint
