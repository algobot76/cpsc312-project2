module Main where

import Lib
import Data.List.Split

promptRow :: Int -> IO [[Char]]
promptRow n = do
    putStrLn (concat ["Row ", (show n), ": "])
    input <- getLine
    let row = splitOn " " input
    return row

main :: IO ()
main = do
    putStrLn "Enter each row of the sudoku, separated by space"
    putStrLn "Enter 0 for unknown elements"
    r1 <- promptRow 1
    let row1 = map (\x -> read x :: Integer) r1
    r2 <- promptRow 2
    let row2 = map (\x -> read x :: Integer) r2
    r3 <- promptRow 3
    let row3 = map (\x -> read x :: Integer) r3
    r4 <- promptRow 4
    let row4 = map (\x -> read x :: Integer) r4
    r5 <- promptRow 5
    let row5 = map (\x -> read x :: Integer) r5
    r6 <- promptRow 6
    let row6 = map (\x -> read x :: Integer) r6
    r7 <- promptRow 7
    let row7 = map (\x -> read x :: Integer) r7
    r8 <- promptRow 8
    let row8 = map (\x -> read x :: Integer) r8
    r9 <- promptRow 9
    let row9 = map (\x -> read x :: Integer) r9
    let sudoku = row1:row2:row3:row4:row5:row6:row7:row8:row9:[]
    putStrLn (show sudoku)
{-main = do
    putStrLn "Enter an incomplete sudoku: "
    input <- getLine
    let sudoku = read input :: [[Integer]]
    print (solve sudoku)
    -}