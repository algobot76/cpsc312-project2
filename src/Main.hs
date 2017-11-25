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

containsZero :: [[Integer]] -> Bool
containsZero [] = False
containsZero (r:rs)
  | 0 `elem` r = True
  | otherwise = containsZero rs
