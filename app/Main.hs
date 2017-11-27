module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter an impcomplete sudoku: "
    input <- getLine
    let arr = read input :: [[Integer]]
    printSudoku arr
