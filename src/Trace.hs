module Trace where

import Syntax
import Pretty

data Trace a = Out (String, a)

instance Functor Trace where
  fmap = undefined

instance Applicative Trace where
  pure    = undefined
  _ <*> _ = undefined

instance Show a => Show (Trace a) where
  show (Out (s,x)) = s ++ show x

instance Monad Trace where
  return x   = Out("", x)
  Out (s, x) >>= f = Out(s ++ s', y)
                      where
                        Out (s', y) = f x


trace :: LExpr -> LExpr -> Trace ()
trace e r = Out ("→" ++ prettyReduce e r ++ "\n", ())
