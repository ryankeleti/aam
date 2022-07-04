module Main where

import CEK
import LambdaParser

main :: IO ()
main = do
  line <- getLine
  case parseExp line of
    Just e -> do
      putStr "parsed: "
      print e
      putStr "eval'd: "
      print $ (\(x, _, _) -> x) (evaluate e)
    Nothing -> putStrLn "failed to parse lambda expression"
