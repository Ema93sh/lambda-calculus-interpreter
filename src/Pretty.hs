{-# LANGUAGE UnicodeSyntax #-}

module Pretty (
      prettyPrint,
      prettyReduce,
      prettyDPrint
      )
    where

import Syntax
import Text.PrettyPrint.ANSI.Leijen

-- TODO: Fix add brackets for applications

class PrettyLambda a where
  pPrint :: a -> Doc
  pReduction ::  a -> a -> Bool -> Doc

instance PrettyLambda LExpr where
  pPrint (Var x)        = char x
  pPrint expr@(App l r)  = printApp l r
  pPrint expr            = hcat [ text "λ",
                                        text $ args expr,
                                        text ".",
                                        pPrint $ body expr
                                      ]


  pReduction (Var x)  _  _          = char x
  pReduction expr redex  s
                  | expr == redex   = underline $ pPrint expr
  pReduction (App l r) redex  _     = rPrintApp l r redex
  pReduction expr@(Abs x e) redex s = hcat [  text "\\",
                                                text $ args expr,
                                                text ".",
                                                pReduction (body expr) redex s
                                              ]

instance PrettyLambda DExpr where
  pPrint (DVal x)        = int x
  pPrint expr@(DApp l r) = printDApp l r
  pPrint (DAbs expr)     = hcat [ text "λ.",
                                  pPrint expr
                                ]
  pReduction = undefined

---------------------------
-- TODO: Refactor --UGLY --
---------------------------

printApp :: LExpr -> LExpr -> Doc
printApp x@(Var _)  y@(Var _) = pPrint x <> pPrint y
printApp x          y@(Var _) = parens (pPrint x) <> pPrint y
printApp x@(Var _)    y       = pPrint x <+> parens (pPrint y)
printApp x            y       = parens (pPrint x) <> parens (pPrint y)

printDApp :: DExpr -> DExpr -> Doc
printDApp x@(DVal _)  y@(DVal _) = pPrint x <+> pPrint y
printDApp x           y@(DVal _) = parens (pPrint x) <+> pPrint y
printDApp x@(DVal _)    y        = pPrint x <+> parens (pPrint y)
printDApp x             y        = parens (pPrint x) <+> parens (pPrint y)


rPrintApp :: LExpr -> LExpr -> LExpr -> Doc
rPrintApp x@(Var _)  y@(Var _) r = pPrint x <> pPrint y
rPrintApp  x         y@(Var _) r = parens (pReduction x r False) <> pPrint  y
rPrintApp x@(Var _)    y       r = pPrint x <+> parens (pReduction y r False)
rPrintApp x            y       r = parens (pReduction x r False) <> parens (pReduction y r False)


prettyPrint :: LExpr -> String
prettyPrint e = displayS ( renderPretty 1 100 (pPrint e) ) ""

prettyDPrint :: DExpr -> String
prettyDPrint e = displayS ( renderPretty 1 100 (pPrint e) ) ""

prettyReduce :: LExpr -> LExpr -> Bool -> String
prettyReduce e r True = displayS ( renderPretty 1 100 (pReduction e r False) ) ""
prettyReduce e r False = displayS ( renderPretty 1 100 (pReduction e r True) ) "<--- Wrong subtitution at this location"

instance Show LExpr where
  show = prettyPrint

instance Show DExpr where
  show = prettyDPrint
