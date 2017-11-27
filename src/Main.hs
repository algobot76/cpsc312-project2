module Main where

main :: IO ()
main = do
  putStrLn "Enter an impcomplete sudoku: "
  input <- getLine
  let arr = read input :: [[Integer]]
  printSudoku arr

printSudoku :: [[Integer]] -> IO ()
printSudoku [] = return ()
printSudoku (r:rs) = do
  printRow r
  putStrLn ""
  printSudoku rs
  where
    printRow :: [Integer] -> IO ()
    printRow [] = return ()
    printRow (x:xs) = do
      putStr (show x)
      putStr " "
      printRow xs

-- Return the row of sudoku at rowIdx
getRow :: [[Integer]] -> Int -> [Integer]
getRow sudoku rowIdx = sudoku !! rowIdx

-- Return the column of sudoku at colIdx
getCol :: [[Integer]] -> Int -> [Integer]
getCol sudoku colIdx = [x !! colIdx | x <- sudoku]

-- Return the submatrix (1-9) of sudoku
getSubMatrix :: [[Integer]] -> Int -> [[Integer]]
getSubMatrix sudoku smNum =
  case smNum of
    1 -> getSubMatrixHelper sudoku 0 0
    2 -> getSubMatrixHelper sudoku 0 3
    3 -> getSubMatrixHelper sudoku 0 6
    4 -> getSubMatrixHelper sudoku 3 0
    5 -> getSubMatrixHelper sudoku 3 3
    6 -> getSubMatrixHelper sudoku 3 6
    7 -> getSubMatrixHelper sudoku 6 0
    8 -> getSubMatrixHelper sudoku 6 3
    9 -> getSubMatrixHelper sudoku 6 6
  where
    getSubMatrixHelper :: [[Integer]] -> Int -> Int -> [[Integer]]
    getSubMatrixHelper _ rowIdx colIdx =
      [ drop colIdx (take (colIdx + 3) (getRow sudoku (rowIdx + offset)))
      | offset <- [0 .. 2]
      ]

-- Check if a sudoku contains 0
containsZero :: [[Integer]] -> Bool
containsZero [] = False
containsZero (r:rs)
  | 0 `elem` r = True
  | otherwise = containsZero rs

-- Check if a list has unique elements
isUnique :: [Integer] -> Bool
isUnique [] = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

-- Check if a sudoku is valid
isValid :: [[Integer]] -> Bool
isValid sudoku =
  not (containsZero sudoku) &&
  and [checkRow sudoku rowIdx | rowIdx <- [0 .. 8]] &&
  and [checkCol sudoku colIdx | colIdx <- [0 .. 8]] &&
  and [checkSubMatrix sudoku smNum | smNum <- [1 .. 9]]
  where
    checkRow :: [[Integer]] -> Int -> Bool
    checkRow _ rowIdx = isUnique (getRow sudoku rowIdx)
    checkCol :: [[Integer]] -> Int -> Bool
    checkCol _ colIdx = isUnique (getCol sudoku colIdx)
    checkSubMatrix :: [[Integer]] -> Int -> Bool
    checkSubMatrix _ smNum = isUnique (concat (getSubMatrix sudoku smNum))
