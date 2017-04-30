{-# LANGUAGE UnicodeSyntax #-}

module Pretty (prettyPrint) where

import Syntax
import Text.PrettyPrint.HughesPJClass

instance Pretty LExpr where
  pPrint (Var x)    = pPrint x
  pPrint (App l r)  = pPrint l <> pPrint r
  pPrint (Abs x e)  = hcat [ text "\\",
                             pPrint x,
                             text ".",
                             pPrint e
                           ]
prettyPrint :: LExpr -> String
prettyPrint = render . pPrint
