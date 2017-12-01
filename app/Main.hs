module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter an incomplete sudoku: "
    input <- getLine
    let sudoku = read input :: [[Integer]]
    print (solve sudoku)