module Main where

-- written on my phone while i was on vacation in miami

import Generator
import Parser
import Types

main :: IO ()
main = do
  putStr "Enter expression: "
  x <- getLine
  case parse x of
    Error e -> putStrLn $ "Error: " ++ e
    Parser expr -> do
      putStrLn $ "Parsed: " ++ show expr
      let mini = minify expr
      putStrLn $ "Minified: " ++ show mini
      putStrLn "Done"
  main