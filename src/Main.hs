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

-- Return the main diagonal of sudoku
getMainDiagonal :: [[Integer]] -> [Integer]
getMainDiagonal sudoku = getMainDiagonalHelper sudoku 0
  where
    getMainDiagonalHelper :: [[Integer]] -> Int -> [Integer]
    getMainDiagonalHelper _ 9 = []
    getMainDiagonalHelper _ n =
      (sudoku !! n) !! n : getMainDiagonalHelper sudoku (n + 1)

-- Return the anti-diagonal of sudoku
getAntidiagonal :: [[Integer]] -> [Integer]
getAntidiagonal sudoku = getAntidiagonalHelper sudoku 0
  where
    getAntidiagonalHelper _ 9 = []
    getAntidiagonalHelper _ n =
      (sudoku !! n) !! (length sudoku - 1 - n) :
      getAntidiagonalHelper sudoku (n + 1)

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
  containsZero sudoku &&
  checkRows sudoku 0 && checkCols sudoku 0 && checkDiagonals sudoku
  where
    checkRows :: [[Integer]] -> Int -> Bool
    checkRows _ 9 = True
    checkRows _ n =
      isUnique (getRow sudoku n) && isUnique (getRow sudoku (n + 1))
    checkCols :: [[Integer]] -> Int -> Bool
    checkCols _ 9 = True
    checkCols _ n =
      isUnique (getCol sudoku n) && isUnique (getCol sudoku (n + 1))
    checkDiagonals :: [[Integer]] -> Bool
    checkDiagonals _ =
      isUnique (getMainDiagonal sudoku) && isUnique (getAntidiagonal sudoku)
