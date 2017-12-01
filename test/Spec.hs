import Test.HUnit
import Lib

--From wikipedia
sampleUnsolved = [[5,3,0,0,7,0,0,0,0],[6,0,0,1,9,5,0,0,0],[0,9,8,0,0,0,0,6,0],[8,0,0,0,6,0,0,0,3],[4,0,0,8,0,3,0,0,1],[7,0,0,0,2,0,0,0,6],[0,6,0,0,0,0,2,8,0],[0,0,0,4,1,9,0,0,5],[0,0,0,0,8,0,0,7,9]]
sampleSolved = [[5,3,4,6,7,8,9,1,2],[6,7,2,1,9,5,3,4,8],[1,9,8,3,4,2,5,6,7],[8,5,9,7,6,1,4,2,3],[4,2,6,8,5,3,7,9,1],[7,1,3,9,2,4,8,5,6],[9,6,1,5,3,7,2,8,4],[2,8,7,4,1,9,6,3,5],[3,4,5,2,8,6,1,7,9]]
--from Sudoku Dragon
sampleImpossible = [[5,1,6,8,4,9,7,3,2],[3,0,7,6,0,5,0,0,0],[8,0,9,7,0,0,0,6,5],[1,3,5,0,6,0,9,0,7],[4,7,2,5,9,1,0,0,6],[9,6,8,3,7,0,0,5,0],[2,5,3,1,8,6,0,7,4],[6,8,4,2,0,7,5,0,0],[7,9,1,0,5,0,6,0,8]]

--unit tests
--s = TestCase (assertEqual "for ," fu (fn))
getNumTest = TestCase (assertEqual "for (getNum sampleSolved 2 2)," 8 (getNum sampleSolved 2 2))
rowTest = TestCase (assertEqual "for (getRow sampleUnsolved 0)," (sampleUnsolved !! 0) (getRow sampleUnsolved 0))
colTest = TestCase (assertEqual "for (getCol sampleSolved 9)," [3,4,5,2,8,6,1,7,9] (getRow sampleSolved 8))
hgtTest = TestCase (assertEqual "for (getHeight sampleSolved)," 9 (getHeight sampleSolved))
wdtTest = TestCase (assertEqual "for (getWidth sampleSolved)," 9 (getWidth sampleSolved))

getSubMatrixtTest1 = TestCase (assertEqual "for (getSubMatrix sampleUnsolved 9)," [[2,8,0],[0,0,5],[0,7,9]] (getSubMatrix sampleUnsolved 9))
cont0Test1 = TestCase (assertEqual "for (containsZero sampleSolved)," False (containsZero sampleSolved))
cont0Test2 = TestCase (assertEqual "for (containsZero sampleSolved)," True (containsZero sampleUnsolved))

setNumTest1 = TestCase (assertEqual "for (setNum sampleSolved 0 0 60)," 60 (((setNum sampleSolved 0 0 60) !! 0 ) !! 0))
setNumTest2 = TestCase (assertEqual "for (setNum sampleSolved 3 4 60)," 60 (((setNum sampleSolved 3 4 60) !! 3 ) !! 4))
setNumTest3 = TestCase (assertEqual "for (setNum sampleSolved 8 8 60)," 60 (((setNum sampleSolved 8 8 60) !! 8 ) !! 8))

unqT1 = TestCase (assertEqual "for (isUnique [1,2,3,4,5])," True (isUnique [1,2,3,4,5]))
unqT2 = TestCase (assertEqual "for (isUnique [1,2,3,4,5,6,2,4,5,2])," False (isUnique [1,2,3,4,5,6,2,4,5,2]))
valT1 = TestCase (assertEqual "for (isValid sampleSolved)," True (isValid sampleSolved))
valT2 = TestCase (assertEqual "for (isValid sampleUnsolved)," False (isValid sampleUnsolved))
valT3 = TestCase (assertEqual "for (isValid [[1,2,3,3],[1]])," False (isValid [[1,2,3,3],[1]]))

cFilT1 = TestCase (assertEqual "for (canFill sampleUnsolved 1 2 6)," False (canFill sampleUnsolved 1 2 6))
cFilT2 = TestCase (assertEqual "for (canFill sampleUnsolved 1 2 2)," True (canFill sampleUnsolved 1 2 2))
mtT1 = TestCase (assertEqual "for (isEmpty sampleSolved 2 4)," False (isEmpty sampleSolved 2 4))
mtT2 = TestCase (assertEqual "for (isEmpty sampleUnsolved 2 4)," True (isEmpty sampleUnsolved 2 4))

gtSmNT1 = TestCase (assertEqual "for (getSMNum 0 0)," 1 (getSMNum 0 0))
gtSmNT2 = TestCase (assertEqual "for (getSMNum 5 0)," 2 (getSMNum 0 4))
gtSmNT3 = TestCase (assertEqual "for (getSMNum 9 0)," 3 (getSMNum 0 8))
gtSmNT4 = TestCase (assertEqual "for (getSMNum 0 4)," 4 (getSMNum 4 0))
gtSmNT5 = TestCase (assertEqual "for (getSMNum 5 4)," 5 (getSMNum 4 4))
gtSmNT6 = TestCase (assertEqual "for (getSMNum 9 4)," 6 (getSMNum 4 8))
gtSmNT7 = TestCase (assertEqual "for (getSMNum 0 9)," 7 (getSMNum 8 0))
gtSmNT8 = TestCase (assertEqual "for (getSMNum 5 9)," 8 (getSMNum 8 4))
gtSmNT9 = TestCase (assertEqual "for (getSMNum 9 9)," 9 (getSMNum 8 8))

satCtrT1 = TestCase (assertEqual "for (satisfiesConstraints sampleSolved)," True (satisfiesConstraints sampleSolved))
satCtrT2 = TestCase (assertEqual "for (satisfiesConstraints sampleUnsolved)," True (satisfiesConstraints sampleUnsolved))
satCtrT3 = TestCase (assertEqual "for (satisfiesConstraints [[1,2,3,3],[1]])," False (satisfiesConstraints [[1,2,3,3],[1]]))

--commented out, pending implementation of Eq for EntryPosition and SudokuSolution
fs0T1 = TestCase (assertEqual "for (firstZero sampleUnsolved)," (Position 2 0) (firstZero sampleUnsolved))
fs0T2 = TestCase (assertEqual "for (firstZero sampleSolved)," NONE (firstZero sampleSolved))
fs0T3 = TestCase (assertEqual "for (firstZero (setNum sampleUnsolved 8 8 0)," (Position 8 8) (firstZero (setNum sampleUnsolved 8 8 0)))
solT1 = TestCase (assertEqual "for (solve sampleUnsolved)," (FoundSolution sampleSolved) (solve sampleUnsolved))
solT2 = TestCase (assertEqual "for (solve sampleImpossible)," UNSAT (solve sampleImpossible))

--unit cases (grouping of unit tests)
--tests = TestList [TestLabel "ttt" ttt, ]
getTests = TestList [TestLabel "getRow" rowTest, TestLabel "getCol" colTest, TestLabel "hgtTest" hgtTest, TestLabel "rowTest" rowTest, TestLabel "wdtTest" wdtTest]
getTests2 = TestList [TestLabel "getSubMatrixtTest1" getSubMatrixtTest1, TestLabel "cont0Test1" cont0Test1, TestLabel "cont0Test2" cont0Test2]
setTests = TestList [TestLabel "setNumTest1" setNumTest1, TestLabel "setNumTest2" setNumTest2, TestLabel "setNumTest3" setNumTest3]
uValTests = TestList [TestLabel "unqT1" unqT1, TestLabel "unqT2" unqT2, TestLabel "valT1" valT1, TestLabel "valT2" valT2, TestLabel "valT3" valT3]
cmtTests = TestList [TestLabel "cFilT1" cFilT1, TestLabel "cFilT2" cFilT2, TestLabel "mtT1" mtT1, TestLabel "mtT2" mtT2]
gtSmTests = TestList [TestLabel "gtSmNT1" gtSmNT1, TestLabel "gtSmNT2" gtSmNT2, TestLabel "gtSmNT3" gtSmNT3, TestLabel "gtSmNT4" gtSmNT4, TestLabel "gtSmNT5" gtSmNT5, TestLabel "gtSmNT6" gtSmNT6, TestLabel "gtSmNT7" gtSmNT7, TestLabel "gtSmNT8" gtSmNT8, TestLabel "gtSmNT9" gtSmNT9]
satCtrTests = TestList [TestLabel "satCtrT1" satCtrT1, TestLabel "satCtrT2" satCtrT2, TestLabel "satCtrT3" satCtrT3]
solTests = TestList [TestLabel "fs0T1" fs0T1, TestLabel "fs0T2" fs0T2, TestLabel "fs0T3" fs0T3, TestLabel "solT1" solT1, TestLabel "solT2" solT2]

main :: IO ()
main = do
    runTestTT getTests
    runTestTT getTests2
    runTestTT setTests
    runTestTT uValTests
    runTestTT cmtTests
    runTestTT gtSmTests
    runTestTT satCtrTests
    runTestTT solTests
    putStrLn "Tests concluded"

{-simpleBoard =
    [ [0, 1, 0, 0, 4, 0, 0, 5, 0]
    , [0, 0, 0, 0, 2, 0, 0, 3, 0]
    , [0, 0, 2, 0, 0, 0, 0, 0, 0]
    , [0, 0, 1, 0, 0, 6, 0, 0, 0]
    , [9, 0, 0, 8, 0, 0, 7, 0, 0]
    , [0, 0, 0, 0, 5, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 2, 0, 0]
    , [0, 0, 0, 1, 0, 0, 5, 0, 0]
    , [1, 0, 0, 0, 0, 0, 0, 0, 7]
    ]-}
