module Main where

import Syntax
import Parser
import Evaluator
import System.IO
import Pretty
import System.Console.Haskeline
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Trans (lift)

strategy "normal"      = normalOrderRedex
strategy "applicative" = applicativeOrderRedex

evaluate :: String -> String -> String
evaluate expr s = case parseExpr expr of
                    Left err -> show "Parse Error! ¯\\_(ツ)_/¯"
                    Right e  -> show $ eval e (strategy s)

explainExpr :: String -> String -> String
explainExpr e1 e2 = case (parseExpr e1, parseExpr e2) of
                    (Right p1, Right p2) -> (explain p1 p2 normalOrderRedex)
                    otherwise -> "Parse Error! ¯\\_(ツ)_/¯"

alphaEq :: String -> String -> String
alphaEq e1 e2 = case (parseExpr e1, parseExpr e2) of
                    (Right p1, Right p2) -> if alphaEquivalence p1  p2 then "Alpha Equivalent" else "Not Alpha Equivalent"
                    otherwise -> "Parse Error! ¯\\_(ツ)_/¯"

debruijnIndex :: String -> String
debruijnIndex expr = case parseExpr expr of
                    Left err -> show "Parse Error! ¯\\_(ツ)_/¯"
                    Right e  -> show $ debruijn e

main =  runStateT (runInputT defaultSettings loop) "normal"

check ma b fb = maybe b fb ma

loop = do
   minput <- getInputLine "λ>"
   check minput (return ()) $ \inp -> do
     let args = words inp
     case args of
       ("quit":_)      -> do outputStrLn "quitting"; return ()
       (":q":_)        -> do outputStrLn "quitting"; return ()
       (":n":_)        -> do outputStrLn $ "setting reduction strategy to normal order"; lift $ put "normal"; loop
       (":a":_)        -> do outputStrLn $ "setting reduction strategy to applicative order"; lift $ put "applicative"; loop
       (":d":e:_)        -> do outputStrLn $ debruijnIndex e; loop
       (":eq":e1:e2:_) -> do outputStrLn $ alphaEq e1 e2; loop
       ("get":_)       -> do v <- lift get; outputStrLn $ "The reduction strategy is " ++ show v; loop
       [expr]          -> do s <- lift get; outputStrLn (evaluate expr s); loop
       (e1:e2:_)       -> do outputStrLn (explainExpr e1 e2); loop
       _               -> do outputStrLn "huh?"; loop
