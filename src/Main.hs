module Main where

import Data.Matrix

main :: IO ()
main = do
  putStrLn "Enter an impcomplete sudoku: "
  input <- getLine
  let arr = read input :: [[Integer]]
  let m = fromLists arr
  print m
