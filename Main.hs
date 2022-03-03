import Logic
import Boards

intToString :: Int -> String
intToString 0 = "_"
intToString 1 = "1"
intToString 2 = "2"
intToString 3 = "3"
intToString 4 = "4"
intToString 5 = "5"
intToString 6 = "6"
intToString 7 = "7"
intToString 8 = "8"
intToString 9 = "9"
intToString x = intToString(div x 10) ++ intToString(mod x 10)


printBoard :: Board -> IO ()
printBoard x = mapM_ putStrLn (startingRow : printBoard' x 0)
    where
        printBoard' [] acc = [seperateRow]
        printBoard' (x:xs) acc | mod acc 3 == 0 = seperateRow : (intToString (acc+1) ++ "  | " ++ printBoard'' x ++ " |") : printBoard' xs (acc+1)
                               | otherwise = (intToString (acc+1) ++ "  | " ++ printBoard'' x ++ " |") : printBoard' xs (acc+1)

        printBoard'' [] = "|"
        printBoard'' [x] = intToString x
        printBoard'' (x:xs) = intToString x ++ " | " ++ printBoard'' xs 

        seperateRow = "   +-----------+-----------+-----------+"
        startingRow = "     1   2   3   4   5   6   7   8   9  "

game :: Board -> IO ()
game board = do
    printBoard board
    if board == solve board then do
        putStrLn "You've won!"
        main
    else do
        putStrLn "Enter row: "
        row <- getLine
        putStrLn "Enter column: "
        column <- getLine
        putStrLn "Value: "
        value <- getLine
        if validInput row column value && validMove board (read row) (read column) (read value)
            then do
                game (Logic.insert board (read row) (read column) (read value))
        else do
            putStrLn "Wrong input. Try again. "
            game board

main :: IO ()
main = do  
    putStrLn "Let's play Sudoku, choose a difficulty. 1 = Easy, 2 = Medium, 3 = Hard, 4 = Expert. "  
    sudokudifficulty <- getLine
    if sudokudifficulty == "1" then do
         game boardEasy
    else if sudokudifficulty == "2" then do
         game boardMedium
    else if sudokudifficulty == "3" then do
         game boardHard
    else if sudokudifficulty == "4" then do
         game boardExpert
    else if sudokudifficulty == "5" then do -- Test
         game boardEasyFinish
    else do
        putStrLn "Wrong input. Try again."
        main
