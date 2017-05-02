module Main where

import Parser
import Evaluator
import System.IO
import Pretty
import System.Console.Haskeline


evaluate :: String -> String
evaluate expr = let lexpr = parseExpr expr in
                case lexpr of
                  Left err -> show err
                  Right e  -> show $ eval e


main :: IO ()
main = runInputT defaultSettings loop
      where
        loop = do
          input <- getInputLine "λ>"
          case input of
            Nothing -> return ()
            Just "quit" -> return ()
            Just ":q" -> return ()
            Just expr -> do outputStrLn $ evaluate expr
                            loop
