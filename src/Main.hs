module Main where

import Parser
import Evaluator
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings repl
  where repl = do
          line <- getInputLine "picolisp> "
          case line of
            Nothing -> return ()
            Just s -> do
              let eithE = parseExpression s
              case eithE of
                Left err -> do
                  outputStrLn $ show err
                  repl
                Right e -> outputStrLn (case eval e of
                  Right out -> show out
                  Left err -> err)
          repl
