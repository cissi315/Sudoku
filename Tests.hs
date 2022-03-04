import Logic
import Boards
import Test.HUnit


-- solve
test1 = TestCase $ assertEqual "solve boardEasy"
            [[5,3,4,8,2,9,7,1,6],[9,6,1,4,5,7,8,2,3],[8,7,2,3,6,1,5,4,9],[2,4,8,9,1,3,6,5,7],[6,1,7,2,8,5,3,9,4],[3,5,9,7,4,6,1,8,2],
             [4,2,3,5,7,8,9,6,1],[1,9,5,6,3,2,4,7,8],[7,8,6,1,9,4,2,3,5]] (Logic.solve boardEasy)

-- solve
test2 = TestCase $ assertEqual "solve boardExpert"
            [[1,3,2,6,5,8,4,7,9],[7,6,4,2,9,1,5,3,8],[5,9,8,7,4,3,1,6,2],[2,5,6,4,3,7,8,9,1],[3,4,9,8,1,5,7,2,6],[8,7,1,9,6,2,3,4,5],
             [9,1,5,3,2,4,6,8,7],[6,8,3,1,7,9,2,5,4],[4,2,7,5,8,6,9,1,3]] (Logic.solve boardExpert)

-- solve
-- an unsolvable Sudoku shall return a empty list
test3 = TestCase $ assertEqual "solve boardImpossible"
            [[]] (Logic.solve boardImpossible)

-- insert
test4 = TestCase $ assertEqual "insert board row column value"
            [[5,3,0,8,2,0,0,1,0],[0,6,1,4,5,7,8,2,3],[8,7,0,0,0,1,0,0,0],[2,4,8,0,0,3,0,5,7],[6,1,0,2,8,0,0,9,0],[0,0,0,7,4,0,0,8,0],
             [4,0,3,5,7,8,9,6,0],[1,0,5,0,0,0,4,7,0],[0,8,0,1,0,4,0,3,5]] (Logic.insert boardEasy 1 1 5)

-- insert
-- the insertion function is able to insert a number greater than 9 but will not pass through other functions to the final board
test5 = TestCase $ assertEqual "insert board row column value"
            [[500,3,0,8,2,0,0,1,0],[0,6,1,4,5,7,8,2,3],[8,7,0,0,0,1,0,0,0],[2,4,8,0,0,3,0,5,7],[6,1,0,2,8,0,0,9,0],[0,0,0,7,4,0,0,8,0],
             [4,0,3,5,7,8,9,6,0],[1,0,5,0,0,0,4,7,0],[0,8,0,1,0,4,0,3,5]] (Logic.insert boardEasy 1 1 500)

-- checkRow
test6 = TestCase $ assertEqual "checkRow board row value"
            True (Logic.checkRow boardEasy 1 5)

-- checkColumn
test7 = TestCase $ assertEqual "checkColumn board column value"
        True (Logic.checkColumn boardEasy 1 5)

-- checkBox
test8 = TestCase $ assertEqual "checkRules board row column value"
            True (Logic.checkBox boardEasy 1 1 5)

-- checkRules
test9 = TestCase $ assertEqual "checkRules board row column value"
            True (Logic.checkRules boardEasy 1 1 5)

-- find0
test10 = TestCase $ assertEqual "find0 board"
            (1,1) (Logic.find0 boardEasy)

-- find0
-- (9,0) is our base case which indicates that all of the cells are occupied
test11 = TestCase $ assertEqual "find0 board"
            (9,0) (Logic.find0 boardEasyFinish)

-- validAnswer
test12 = TestCase $ assertEqual "validAnswer board row column value"
            True (Logic.validAnswer boardEasy 1 1 5)

-- validAnswer
test13 = TestCase $ assertEqual "validAnswer board row column value"
            False (Logic.validAnswer boardEasy 1 1 6)

-- validInput
test14 = TestCase $ assertEqual "validInput row column value"
           True (Logic.validInput "1" "1" "5")

-- validInput
test15 = TestCase $ assertEqual "validInput row column value"
           False (Logic.validInput "10" "0" "a")

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15] 
