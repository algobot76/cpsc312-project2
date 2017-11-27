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

-- Return the number at (rowIdx, colIdx)
getNum :: [[Integer]] -> Int -> Int -> Integer
getNum sudoku rowIdx colIdx = (sudoku !! rowIdx) !! colIdx

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

-- Check if a number can be filled in a slot (rowIdx,colIdx)
canFill :: [[Integer]] -> Int -> Int -> Integer -> Bool
canFill sudoku rowIdx colIdx num =
  isAvailable sudoku rowIdx colIdx &&
  checkRow sudoku rowIdx colIdx num &&
  checkCol sudoku rowIdx colIdx num && checkSubMatrix sudoku rowIdx colIdx num
  where
    isAvailable :: [[Integer]] -> Int -> Int -> Bool
    isAvailable _ _ _ =
      0 `elem` getRow sudoku rowIdx &&
      0 `elem` getCol sudoku colIdx &&
      0 `elem` concat (getSubMatrix sudoku (getSMNum rowIdx colIdx))
    checkRow :: [[Integer]] -> Int -> Int -> Integer -> Bool
    checkRow _ _ _ _ = num `notElem` getRow sudoku rowIdx
    checkCol :: [[Integer]] -> Int -> Int -> Integer -> Bool
    checkCol _ _ _ _ = num `notElem` getCol sudoku colIdx
    checkSubMatrix :: [[Integer]] -> Int -> Int -> Integer -> Bool
    checkSubMatrix _ _ _ _ =
      num `notElem` concat (getSubMatrix sudoku (getSMNum rowIdx colIdx))

-- Check if a slot at (rowIdx, colIdx) is empty
isEmpty :: [[Integer]] -> Int -> Int -> Bool
isEmpty sudoku rowIdx colIdx = 0 == getNum sudoku rowIdx colIdx

-- Return the submatrix number based on rowIdx and colIdx
getSMNum :: Int -> Int -> Int
getSMNum rowIdx colIdx
  | rowIdx < 3 && colIdx < 3 = 1
  | rowIdx < 3 && colIdx > 2 && colIdx < 6 = 2
  | rowIdx < 3 && colIdx > 5 = 3
  | rowIdx > 2 && rowIdx < 6 && colIdx < 3 = 4
  | rowIdx > 2 && rowIdx < 6 && colIdx > 2 && colIdx < 6 = 5
  | rowIdx > 2 && rowIdx < 6 && colIdx > 5 = 6
  | rowIdx > 5 && colIdx < 3 = 7
  | rowIdx > 5 && colIdx > 2 && colIdx < 6 = 8
  | otherwise = 9
