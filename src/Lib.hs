module Lib where

-- Return the number at (rowIdx, colIdx)
getNum :: [[Integer]] -> Int -> Int -> Integer
getNum sudoku rowIdx colIdx = (sudoku !! rowIdx) !! colIdx

-- Fill the number in slot at (rowIdx, colIdx)
setNum :: [[Integer]] -> Int -> Int -> Integer -> [[Integer]]
setNum sudoku rowIdx colIdx num =
  take rowIdx sudoku
    ++ [replace (sudoku !! rowIdx) colIdx num]
    ++ drop (rowIdx + 1) sudoku
 where
  replace :: [Integer] -> Int -> Integer -> [Integer]
  replace row _ _ = take colIdx row ++ [num] ++ drop (colIdx + 1) row

-- Return the row of sudoku at rowIdx
getRow :: [[Integer]] -> Int -> [Integer]
getRow sudoku rowIdx = sudoku !! rowIdx

-- Return the column of sudoku at colIdx
getCol :: [[Integer]] -> Int -> [Integer]
getCol sudoku colIdx = [ x !! colIdx | x <- sudoku ]

-- Return the height of the sudoku board
getHeight :: [[Integer]] -> Int
getHeight = length

-- Return the width of the sudoku board
getWidth :: [[Integer]] -> Int
getWidth sudoku = length $ getRow sudoku 0

-- Return the submatrix (1-9) of sudoku
getSubMatrix :: [[Integer]] -> Int -> [[Integer]]
getSubMatrix sudoku smNum = case smNum of
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
containsZero (r:rs) | 0 `elem` r = True
                    | otherwise  = containsZero rs

-- Check if a list has unique elements
isUnique :: [Integer] -> Bool
isUnique []     = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

-- Check if a sudoku is valid
isValid :: [[Integer]] -> Bool
isValid sudoku = not (containsZero sudoku) && satisfiesConstraints sudoku

-- Check if a sudoku satisfies sudoku constraints (but can have zeroes)
satisfiesConstraints :: [[Integer]] -> Bool
satisfiesConstraints sudoku =
  and [ checkRow sudoku rowIdx | rowIdx <- [0 .. 8] ]
    && and [ checkCol sudoku colIdx | colIdx <- [0 .. 8] ]
    && and [ checkSubMatrix sudoku smNum | smNum <- [1 .. 9] ]
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
  isAvailable sudoku rowIdx colIdx
    && checkRow       sudoku rowIdx colIdx num
    && checkCol       sudoku rowIdx colIdx num
    && checkSubMatrix sudoku rowIdx colIdx num
 where
  isAvailable :: [[Integer]] -> Int -> Int -> Bool
  isAvailable _ _ _ =
    0
      `elem` getRow sudoku rowIdx
      &&     0
      `elem` getCol sudoku colIdx
      &&     0
      `elem` concat (getSubMatrix sudoku (getSMNum rowIdx colIdx))
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
  | rowIdx < 3 && colIdx < 3               = 1
  | rowIdx < 3 && colIdx > 2 && colIdx < 6 = 2
  | rowIdx < 3 && colIdx > 5               = 3
  | rowIdx > 2 && rowIdx < 6 && colIdx < 3 = 4
  | rowIdx > 2 && rowIdx < 6 && colIdx > 2 && colIdx < 6 = 5
  | rowIdx > 2 && rowIdx < 6 && colIdx > 5 = 6
  | rowIdx > 5 && colIdx < 3               = 7
  | rowIdx > 5 && colIdx > 2 && colIdx < 6 = 8
  | otherwise                              = 9

data EntryPosition = Position Int Int | NONE deriving (Eq, Show)
  isNONE NONE           = True
  isNONE (Position a b) = False
  getX (Position a b) = a
  getY (Position a b) = b

instance Show EntryPosition where
  show NONE = "NONE"
  show Position x y = "Position " ++ x ++ y

instance Eq EntryPosition where
  (==) (Position a b) (Position c d) = (a == c) && (b == d)
  (==) _ _ = False

-- firstZero: finds (x,y) location of first zero, or NONE
firstZero :: [[Integer]] -> EntryPosition
firstZero sudoku = firstZeroHelper sudoku 0 0

firstZeroHelper :: [[Integer]] -> Int -> Int -> EntryPosition
firstZeroHelper sudoku y x | getNum sudoku x y == 0 = Position x y
                           | otherwise = firstZeroHelper sudoku y (x + 1)
 where
  nextx | x < (getWidth sudoku) - 1 = x + 1
        | otherwise                 = 0
  nexty | nextx == 0 = y + 1
        | otherwise  = y

-- zeros: constructs a matrix of zeros
zeros :: Int -> Int -> [[Integer]]
zeros w h = [ [ 0 | x <- [1 .. w] ] | y <- [1 .. h] ]

-- Brute Force solver: fills in zeros, or returns impossible
data FoundSolution = [[Integer]]

data SudokuSolution = FoundSolution [[Integer]] | UNSAT deriving (Eq, Show)
instance Show SudokuSolution where
  show (FoundSolution sudoku) =  sudoku2Str sudoku
  show UNSAT = "Solution not found :("
instance Eq SudokuSolution where
  (==) (FoundSolution x) (FoundSolution y) = (x == y)
  (==) _ _ = False

sudoku2Str :: [[Integer]] -> String
sudoku2Str []     = ""
sudoku2Str (r:rs) = row2Str r ++ "\n" ++ sudoku2Str rs
 where
  row2Str :: [Integer] -> String
  row2Str []     = ""
  row2Str (x:xs) = show x ++ " " ++ row2Str xs

solve :: [[Integer]] -> SudokuSolution
solve sudoku = solveHelper sudoku $ zeros (getHeight sudoku) (getWidth sudoku)

solveHelper :: [[Integer]] -> [[Integer]] -> SudokuSolution
solveHelper sudoku scratch
  | isValid scratch = FoundSolution scratch
  | (isUNSAT nextScratch) = UNSAT
  | (satisfiesConstraints scratch) && (matches scratch sudoku) = increment
    sudoku
    scratch
    False
  | otherwise = solveHelper sudoku (getMatrix nextScratch)
  where nextScratch = increment sudoku scratch True

-- increment: checks if scratch board can be incremented given a constraining sudoku puzzle
increment :: [[Integer]] -> [[Integer]] -> Bool -> SudokuSolution
increment sudoku scratch backtrack
  | isNONE fz = UNSAT
  | otherwise = incrementHelper sudoku scratch (getX fz) (getY fz)
  where fz = firstZero scratch

incrementHelper :: [[Integer]] -> [[Integer]] -> Int -> Int -> SudokuSolution
incrementHelper sudoku scratch y x
  | unsat = UNSAT
  | (satisfiesConstraints scratch) && (matches sudoku scratch) = FoundSolution
    scratch
  | fixed = incrementHelper sudoku scratch nextx nexty
  | num < 9 = FoundSolution $ setNum sudoku y x (1 + getNum sudoku y x)
  | otherwise = incrementHelper sudoku (setNum sudoku x y 0) nextx nexty
 where
  num   = getNum scratch y x
  fixed = (getNum sudoku y x) > 0
  nextx | x > 0     = x - 1
        | otherwise = (getWidth sudoku) - 1
  nexty | x > 0     = y
        | otherwise = y - 1
  unsat = nexty < 0

-- matches: checks that the non-zero terms agree
matches :: [[Integer]] -> [[Integer]] -> Bool
matches a b = not (any (\(a, b) -> (a == 0 || b == 0 || a == b)) (zip fa fb))
 where
  fa = concat a
  fb = concat b

isUNSAT :: SudokuSolution -> Bool
isUNSAT UNSAT                  = True
isUNSAT (FoundSolution matrix) = False

getMatrix (FoundSolution matrix) = matrix
