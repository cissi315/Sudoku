import Sudoku
import SudokuExample

type Board = [[Int]]

stringNumber :: Int -> String
stringNumber 0 = "_"
stringNumber 1 = "1"
stringNumber 2 = "2"
stringNumber 3 = "3"
stringNumber 4 = "4"
stringNumber 5 = "5"
stringNumber 6 = "6"
stringNumber 7 = "7"
stringNumber 8 = "8"
stringNumber 9 = "9"
stringNumber x = stringNumber(div x 10) ++ stringNumber(mod x 10)


printBoard :: Board -> IO ()
printBoard x = mapM_ putStrLn (startingRow : printBoard' x 0)     
    where
        printBoard' [] acc = [seperateRow]
        printBoard' (x:xs) acc | mod acc 3 == 0 = seperateRow : (stringNumber (acc+1) ++ "  | " ++ printBoard'' x ++ " |") : printBoard' xs (acc+1)
                               | otherwise = (stringNumber (acc+1) ++ "  | " ++ printBoard'' x ++ " |") : printBoard' xs (acc+1)

        printBoard'' [] = "|"
        printBoard'' [x] = stringNumber x
        printBoard'' (x:xs) = stringNumber x ++ " | " ++ printBoard'' xs 

        seperateRow = "   +-----------+-----------+-----------+"
        startingRow = "     1   2   3   4   5   6   7   8   9  "

game :: Board -> IO ()
game board = do
    printBoard board
    if board == Sudoku.solve board then do
        putStrLn "You've won!"
        main
    else do
        putStrLn "Enter row: "
        row <- getLine
        putStrLn "Enter column: "
        column <- getLine
        putStrLn "Value: "
        value <- getLine
        if Sudoku.validMove board (read row) (read column) (read value)
            then do
                game (Sudoku.insert board (read row) (read column) (read value))
        else do
            putStrLn "Wrong input. Try again. "
            game board
    
main = do  
    putStrLn "Let's play Sudoku, choose a difficulty. 1 = Easy, 2 = Medium, 3 = Hard, 4 = Expert. "  
    sudokudifficulty <- getLine
    if sudokudifficulty == "1" then do
         game SudokuExample.boardEasy
    else if sudokudifficulty == "2" then do
         game SudokuExample.boardMedium
    else if sudokudifficulty == "3" then do
         game SudokuExample.boardHard
    else if sudokudifficulty == "4" then do
         game SudokuExample.boardExpert
    else if sudokudifficulty == "5" then do -- Test
         game SudokuExample.boardEasyFinish
    else do
        putStrLn "Wrong input. Try again."
        main