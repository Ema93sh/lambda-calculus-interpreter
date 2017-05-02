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
  pPrint :: a -> Doc
  pReduction ::  a -> a -> Doc

instance PrettyLambda LExpr where
  pPrint (Var x)        = char x
  pPrint expr@(App l r)  = printApp l r
  pPrint expr            = hcat [ text "\\",
                                        text $ args expr,
                                        text ".",
                                        pPrint $ body expr
                                      ]


  pReduction (Var x)  _            = char x
  pReduction expr redex
                  | expr == redex  = underline $ pPrint expr
  pReduction (App l r) redex       = rPrintApp l r redex
  pReduction expr@(Abs x e) redex  = hcat [  text "\\",
                                                text $ args expr,
                                                text ".",
                                                pReduction (body expr) redex
                                              ]


-- TODO UGLY
printApp :: LExpr -> LExpr -> Doc
printApp x@(Var _)  y@(Var _) = pPrint x <> pPrint y
printApp x          y@(Var _) = parens (pPrint x) <> pPrint y
printApp x@(Var _)    y       = pPrint x <+> parens (pPrint y)
printApp x            y       = parens (pPrint x) <> parens (pPrint y)

rPrintApp :: LExpr -> LExpr -> LExpr -> Doc
rPrintApp x@(Var _)  y@(Var _) r = pPrint x <> pPrint y
rPrintApp  x         y@(Var _) r = parens (pReduction x r) <> pPrint  y
rPrintApp x@(Var _)    y       r = pPrint x <+> parens (pReduction y r)
rPrintApp x            y       r = parens (pReduction x r) <> parens (pReduction y r)



prettyPrint :: LExpr -> String
prettyPrint e = displayS ( renderPretty 1 100 (pPrint e) ) ""

prettyReduce :: LExpr -> LExpr -> String
prettyReduce e r = displayS ( renderPretty 1 100 (pReduction e r) )""

instance Show LExpr where
  show = prettyPrint
