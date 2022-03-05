module Logic where 


{- a Sudoku board conisting of numbers represented as lists whithin a list.
 -}
type Board = [[Int]]

 {- solve board
     A function that combines multiple functions to solve a Sudoku board using backtracking. 
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0.
     RETURNS: A finished sudoku board or an empty list if it's not possible to solve given Sudoku board.
     EXAMPLES: solve boardEasy == [[5,3,4,8,2,9,7,1,6],[9,6,1,4,5,7,8,2,3],[8,7,2,3,6,1,5,4,9],[2,4,8,9,1,3,6,5,7],
                                  [6,1,7,2,8,5,3,9,4],[3,5,9,7,4,6,1,8,2],[4,2,3,5,7,8,9,6,1],[1,9,5,6,3,2,4,7,8],[7,8,6,1,9,4,2,3,5]]
                Note: boardEasy can be found in file "Boards".
  -}
solve :: Board -> Board
-- VARIANT: length board
solve board 
    | find0 board == (9,0) = board
    | otherwise = solve' board (find0 board) 1
    where
        solve' board (row,column) 10 = [[]]
        solve' board (row,column) value
            | checkRules board row column value && x /= [[]] = x
            | otherwise = solve' board (row,column) (value+1)
                where
                    x = solve (insert board row column value)

{- insert board row column value
     A function that inserts, or rather replace, a value into Board.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0. Row and column must be a int between 1..9.
     RETURNS: A modified board with value replaced the intended cell.
     EXAMPLES: insert boardEasy 1 1 5 == [[5,3,0,8,2,0,0,1,0],[0,6,1,4,5,7,8,2,3],[8,7,0,0,0,1,0,0,0],[2,4,8,0,0,3,0,5,7],
                                         [6,1,0,2,8,0,0,9,0],[0,0,0,7,4,0,0,8,0],[4,0,3,5,7,8,9,6,0],[1,0,5,0,0,0,4,7,0],[0,8,0,1,0,4,0,3,5]]
                Note: boardEasy can be found in file "Boards".
  -}
insert :: Board -> Int -> Int -> Int -> Board
insert [] _ _ _ = []
insert board row column value = take (row - 1) board ++ insert' (board !! (row-1)) column value : drop row board
    where 
          insert' [] _ _ = []
          insert' board column value = take (column - 1) board ++ value : drop column board

{- checkRow board row value
     A function that checks if value doesn't exist within a specified row in the board.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0. Row must be a int between 1..9.
     RETURNS: A boolean answer, if the value exist within a specified row it returns False otherwise True.  
     EXAMPLES: checkRow boardEasy 1 5 == True
               checkRow boardEasy 1 3 == False
                Note: boardEasy can be found in file "Boards".
  -}
checkRow :: Board -> Int -> Int -> Bool
checkRow board row value = value `notElem` (board !! (row - 1))

{- checkColumn board column value
     A function that checks if value doesn't exist within a specified column in the board.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0. Column must be a int between 1..9.
     RETURNS: A boolean answer, if the value exist within a specified column it returns False otherwise True.
     EXAMPLES: checkColumn boardEasy 1 5 == True
               checkColumn boardEasy 1 6 == False
                Note: boardEasy can be found in file "Boards".
  -}
checkColumn :: Board -> Int -> Int -> Bool
-- VARIANT: length board
checkColumn board column value = value `notElem` checkColumn' board column 
   where 
      checkColumn' [] _ = []
      checkColumn' (x:xs) column = (x !! (column-1)) : checkColumn' xs column

{- checkBox board row column value
     A function that checks if value doesn't exist within a specified "box" in the board.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0. Row and column must be a int between 1..9.
     RETURNS: A boolean answer, if the value exist within a specified "box" it returns False otherwise True
     EXAMPLES: checkBox boardEasy 1 1 5 == True
               checkBox boardEasy 1 1 6 == False
                Note: boardEasy can be found in file "Boards".
  -}
checkBox :: Board -> Int -> Int -> Int -> Bool
-- VARIANT: length board
checkBox board row column value
    | row <= 3 && row > 0 && column <= 3 && column > 0 = value `notElem` checkBox' board 1 1
    | row <= 6 && row > 3 && column <= 3 && column > 0 = value `notElem` checkBox' board 4 1
    | row <= 9 && row > 6 && column <= 3 && column > 0 = value `notElem` checkBox' board 7 1
    | row <= 3 && row > 0 && column <= 6 && column > 3 = value `notElem` checkBox' board 1 4
    | row <= 6 && row > 3 && column <= 6 && column > 3 = value `notElem` checkBox' board 4 4
    | row <= 9 && row > 6 && column <= 6 && column > 3 = value `notElem` checkBox' board 7 4
    | row <= 3 && row > 0 && column <= 9 && column > 6 = value `notElem` checkBox' board 1 7
    | row <= 6 && row > 3 && column <= 9 && column > 6 = value `notElem` checkBox' board 4 7
    | otherwise = value `notElem` checkBox' board 7 7
        where
            checkBox' board row column = take 3 (drop (column-1) (board !! (row - 1))) ++ take 3 (drop (column-1) (board !! row)) ++
             take 3 (drop (column-1) (board !! (row+1)))

{- checkRules board row column value
     A function that combines multiple functions, checkRow, checkColumn and checkBox, and checks if all of the condition are True.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0. Row and column must be a int between 1..9.
     RETURNS: A boolean answer depending if all of the conditions in checkRow, checkColumn and checkBox returns a boolean value True.  
     EXAMPLES: checkRules boardEasy 1 1 5 == True
               checkRules boardEasy 1 1 6 == False
                Note: boardEasy can be found in file "Boards".
  -}
checkRules :: Board -> Int -> Int -> Int -> Bool
checkRules board row column value = checkBox board row column value && checkColumn board column value && checkRow board row value 

{- find0 board
     A function that finds the first empty cell, a zero, in the sudoku board. Starting from upper left side.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0.
     RETURNS:  The coordinates of the first empty cell as a tuple, (row,column). 
     EXAMPLES: find0 emptyBoard == (1,1)
  -}
find0 :: Board -> (Int, Int)
-- VARIANT: length board
find0 board = (findRow board, findColumn (board !! (findRow board - 1)))
    where
        findRow [] = 0
        findRow board
            | 0 `elem` (board !! 0) = 1 
            | 0 `elem` (board !! 1) = 2
            | 0 `elem` (board !! 2) = 3
            | 0 `elem` (board !! 3) = 4
            | 0 `elem` (board !! 4) = 5
            | 0 `elem` (board !! 5) = 6
            | 0 `elem` (board !! 6) = 7
            | 0 `elem` (board !! 7) = 8
            | otherwise = 9

        findColumn [] = 0
        findColumn (x:xs) 
            | x == 0 = 9 - length xs
            | otherwise = findColumn xs

-- IO auxiliary functions

{- validAnswer board row column value
    A function that checks if inserted value is the right answer within given cell.
     PRE:  The Sudoku board must be the standard dimensions by 9X9 cells. Empty cells must be represented as int 0. Row and column must be a int between 1..9.
     RETURNS: A boolean answer True if the value is equal to the right answer, otherwise False.
     EXAMPLES: validAnswer boardEasy 1 1 5 == True
               validAnswer boardEasy 1 1 6 == False
                Note: boardEasy can be found in file "Boards".
  -}
validAnswer :: Board -> Int -> Int -> Int -> Bool
-- VARIANT: length board
validAnswer board row column value
    | (board !! (row-1) !! (column-1)) /= 0 = False 
    | otherwise = validAnswer' (solve board) row column value
        where
            validAnswer' board row column value 
                | (board !! (row-1) !! (column-1)) == value = True 
                | otherwise = False

{- validInput row column value
     A function checks if inserted row, column, and value is a string between 1..9. 
     RETURNS: A boolean answer depending if the typed answer is a string between 1..9 or not.
     EXAMPLES: validInput "1" "1" "1" == True
               validInput "10" "0" "a" == False
  -}
validInput :: String -> String -> String -> Bool
validInput row column value = (row `elem` list) && (column `elem` list) && (value `elem` list)
    where
        list = map show [1..9]
