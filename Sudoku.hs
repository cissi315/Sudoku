--import Gistfile1

type Board = [[Int]]

--source = Gistfile1.main

{-main = do  
    putStrLn "Insert Soduko"  
    sudokuinput <- getLine  
    putStrLn (sudokuinput)-}

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

example :: Board
example = [[5, 4, 0, 9, 2, 0, 8, 0, 6],
           [2, 1, 9, 6, 0, 7, 0, 4, 3],
           [0, 7, 0, 3, 5, 0, 2, 1, 0],
           
           [9, 0, 7, 4, 0, 5, 3, 2, 1],
           [0, 6, 1, 7, 9, 0, 6, 0, 4],
           [6, 5, 4, 1, 3, 2, 0, 8, 0],
           
           [7, 6, 5, 0, 4, 3, 1, 9, 0],
           [4, 0, 2, 8, 1, 9, 7, 0, 5],
           [1, 9, 0, 5, 7, 0, 4, 3, 2]]
--solve ::

checkRow :: Board -> Int -> Int -> Bool
checkRow board row value = value `notElem` (board !! (row - 1))

{-
checkColumn1 :: Board -> Int -> Int -> Bool
checkColumn1 board column value = checkColumn' board 0 column value
    where 
        checkColumn' board 9 column value = True
        checkColumn' board row column value 
            | value == (board !! row !! column) = False
            | otherwise = checkColumn' board (row+1) column value 
-}

checkColumn :: Board -> Int -> Int -> Bool
checkColumn board column value = value `notElem` checkColumn' board column 
   where 
      checkColumn' [] _ = []
      checkColumn' (x:xs) column = (x !! (column-1)) : checkColumn' xs column

checkBox :: Board -> Int -> Int -> Int -> Bool
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
            checkBox' board row column = take 3 (drop (column-1) (board !! (row - 1))) ++ take 3 (drop (column-1) (board !! row)) ++ take 3 (drop (column-1) (board !! (row+1)))

checkRules :: Board -> Int -> Int -> Int -> Bool
checkRules board row column value = checkBox board row column value && checkColumn board column value && checkRow board row value 


