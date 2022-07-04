module Main where

import CESK
import LambdaParser

main :: IO ()
main = do
  line <- getLine
  case parseExp line of
    Just e -> do
      putStr "parsed: "
      print e
      putStr "eval'd: "
      print $ (\(x, _, _, _) -> x) (evaluate e)
    Nothing -> putStrLn "failed to parse lambda expression"
