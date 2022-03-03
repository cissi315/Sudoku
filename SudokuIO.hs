import Sudoku

type Board = [[Int]]

boardempty :: Board
boardempty = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]

exampleEasy :: Board
exampleEasy = [[0,3,0,8,2,0,0,1,0],
           [0,6,1,4,5,7,8,2,3],
           [8,7,0,0,0,1,0,0,0],
           [2,4,8,0,0,3,0,5,7],
           [6,1,0,2,8,0,0,9,0],
           [0,0,0,7,4,0,0,8,0],
           [4,0,3,5,7,8,9,6,0],
           [1,0,5,0,0,0,4,7,0],
           [0,8,0,1,0,4,0,3,5]]

exampleEasyFinish :: Board
exampleEasyFinish = [
           [5,3,4,8,2,9,7,1,6],
           [9,6,1,4,5,7,8,2,3],
           [8,7,2,3,6,1,5,4,9],
           [2,4,8,9,1,3,6,5,7],
           [6,1,7,2,8,5,3,9,4],
           [3,5,9,7,4,6,1,8,2],
           [4,2,3,5,7,8,9,6,1],
           [1,9,5,6,3,2,4,7,8],
           [7,8,6,1,9,4,2,3,5]]

exampleHard :: Board
exampleHard = [
           [0,0,8,1,5,0,9,0,2],
           [0,2,0,0,0,0,0,5,0],
           [0,0,0,0,2,0,1,6,4],
           [0,8,0,6,0,0,0,0,0],
           [0,0,3,4,0,8,2,0,0],
           [0,0,0,0,0,9,0,1,0],
           [3,5,9,0,8,0,0,0,0],
           [0,4,0,0,0,0,0,2,0],
           [2,0,6,0,9,4,3,0,0]]

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
    putStrLn "Let's play Sudoku, choose a difficulty."  
    sudokuinput <- getLine  
    game exampleEasy 