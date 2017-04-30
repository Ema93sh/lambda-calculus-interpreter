module Main where

import Parser
import Evaluator
import Pretty
import System.IO

import System.Console.Haskeline


eval :: String -> String
eval expr = let lexpr = parseExpr expr in
            case lexpr of
              Left err -> show err
              Right e -> (prettyPrint . betaReduction) e


main :: IO ()
main = runInputT defaultSettings loop
      where
        loop = do
          input <- getInputLine "Lambda>"
          case input of
            Nothing -> return ()
            Just "quit" -> return ()
            Just ":q" -> return ()
            Just expr -> do outputStrLn $ eval expr
                            loop
