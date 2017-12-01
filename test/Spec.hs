import Test.HUnit
import Lib

--From wikipedia
sampleUnsolved = [[5,3,0,0,7,0,0,0,0],[6,0,0,1,9,5,0,0,0],[0,9,8,0,0,0,0,6,0],[8,0,0,0,6,0,0,0,3],[4,0,0,8,0,3,0,0,1],[7,0,0,0,2,0,0,0,6],[0,6,0,0,0,0,2,8,0],[0,0,0,4,1,9,0,0,5],[0,0,0,0,8,0,0,7,9]]
sampleSolved = [[5,3,4,6,7,8,9,1,2],[6,7,2,1,9,5,3,4,8],[1,9,8,3,4,2,5,6,7],[8,5,9,7,6,1,4,2,3],[4,2,6,8,5,3,7,9,1],[7,1,3,9,2,4,8,5,6],[9,6,1,5,3,7,2,8,4],[2,8,7,4,1,9,6,3,5],[3,4,5,2,8,6,1,7,9]]

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
valT2 = TestCase (assertEqual "for (isValid sampleUnsolved)," True (isValid sampleUnsolved))
valT3 = TestCase (assertEqual "for (isValid sampleUnsolved)," True (isValid [[1,2,3,3],[1]]))

cFilT1 = TestCase (assertEqual "for (canFill sampleUnsolved 1 2 6)," False (canFill sampleUnsolved 1 2 6))
cFilT2 = TestCase (assertEqual "for (canFill sampleUnsolved 1 2 2)," True (canFill sampleUnsolved 1 2 2))
mtT1 = TestCase (assertEqual "for (isEmpty sampleSolved 2 4)," False (isEmpty sampleSolved 2 4))
mtT2 = TestCase (assertEqual "for (isEmpty sampleUnsolved 2 4)," True (isEmpty sampleUnsolved 2 4))

--unit cases (grouping of unit tests)
getTests = TestList [TestLabel "getRow" rowTest, TestLabel "getCol" colTest, TestLabel "hgtTest" hgtTest, TestLabel "rowTest" rowTest, TestLabel "wdtTest" wdtTest]
getTests2 = TestList [TestLabel "getSubMatrixtTest1" getSubMatrixtTest1, TestLabel "cont0Test1" cont0Test1, TestLabel "cont0Test2" cont0Test2]
setTests = TestList [TestLabel "setNumTest1" setNumTest1, TestLabel "setNumTest2" setNumTest2, TestLabel "setNumTest3" setNumTest3]
uValTests = TestList [TestLabel "unqT1" unqT1, TestLabel "unqT2" unqT2, TestLabel "valT1" valT1, TestLabel "valT2" valT2, TestLabel "valT3" valT3]
cmtTests = TestList [TestLabel "cFilT1" cFilT1, TestLabel "cFilT2" cFilT2, TestLabel "mtT1" mtT1, TestLabel "mtT2" mtT2]


main :: IO ()
main = do
    runTestTT getTests
    runTestTT getTests2
    runTestTT setTests
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
