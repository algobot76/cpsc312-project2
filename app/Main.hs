module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter an impcomplete sudoku: "
    input <- getLine
    let sudoku = read input :: [[Integer]]
    print (solve sudoku)
