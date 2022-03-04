module Main(printBoard, game, main) where 
import Logic
import Boards

{- printBoard board
     An IO function that prints a more aesthetically and intiutive pleasing Sudoku board instead of the initial lists within lists layout.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0.
     SIDE EFFECTS: Prints out the Sudoku in the terminal.
     EXAMPLES: printBoard boardEasy ==       1   2   3   4   5   6   7   8   9  
                                           +-----------+-----------+-----------+
                                        1  | _ | 3 | _ | 8 | 2 | _ | _ | 1 | _ |
                                        2  | _ | 6 | 1 | 4 | 5 | 7 | 8 | 2 | 3 |
                                        3  | 8 | 7 | _ | _ | _ | 1 | _ | _ | _ |
                                           +-----------+-----------+-----------+
                                        4  | 2 | 4 | 8 | _ | _ | 3 | _ | 5 | 7 |
                                        5  | 6 | 1 | _ | 2 | 8 | _ | _ | 9 | _ |
                                        6  | _ | _ | _ | 7 | 4 | _ | _ | 8 | _ |
                                           +-----------+-----------+-----------+
                                        7  | 4 | _ | 3 | 5 | 7 | 8 | 9 | 6 | _ |
                                        8  | 1 | _ | 5 | _ | _ | _ | 4 | 7 | _ |
                                        9  | _ | 8 | _ | 1 | _ | 4 | _ | 3 | 5 |
                                           +-----------+-----------+-----------+
               Note: boardEasy can be found in file "Boards".
  -}
printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn (startingRow : printBoard' board 0)
    where
        printBoard' [] acc = [seperateRow]
        printBoard' (x:xs) acc | mod acc 3 == 0 = seperateRow : (intToString (acc+1) ++ "  | " ++ printBoard'' x ++ " |") : printBoard' xs (acc+1)
                               | otherwise = (intToString (acc+1) ++ "  | " ++ printBoard'' x ++ " |") : printBoard' xs (acc+1)

        printBoard'' [] = "|"
        printBoard'' [x] = intToString x
        printBoard'' (x:xs) = intToString x ++ " | " ++ printBoard'' xs

        intToString 0 = "_"
        intToString x = show x

        seperateRow = "   +-----------+-----------+-----------+"
        startingRow = "     1   2   3   4   5   6   7   8   9  "

{- game board 
     An IO function that progress the game. It process the player's inputs and also determines if he or she has won.
     PRE:  The imported sudoku must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0.
     SIDE EFFECTS: Prints out the current board state and questions regardning which row, column, and value the player wants to insert. 
                   Restarts program when the player has won.
  -}
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
        if validInput row column value && validAnswer board (read row) (read column) (read value)
            then do
                game (Logic.insert board (read row) (read column) (read value))
        else do
            putStrLn "Wrong input. Try again. "
            game board

{- main
     An IO function that initiate the game and ask difficulty.
     SIDE EFFECTS: Prints out questions regardning difficulty and then initiate the game within the terminal.
  -}
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
    else do
        putStrLn "Wrong input. Try again."
        main
