--import Gistfile1

type Board = [[Int]]

--source = Gistfile1.main

{-main = do  
    putStrLn "Insert Soduko"  
    sudokuinput <- getLine  
    putStrLn (sudokuinput)-}

<<<<<<< HEAD
boardempty :: Board 
=======
boardempty :: Board
>>>>>>> 9342ef9c22ea98d934e24c4158eb30c72560b311
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
<<<<<<< HEAD
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
=======
example = [[0,3,0,8,2,0,0,1,0],
           [0,6,1,4,5,7,8,2,3],
           [8,7,0,0,0,1,0,0,0],
           [2,4,8,0,0,3,0,5,7],
           [6,1,0,2,8,0,0,9,0],
           [0,0,0,7,4,0,0,8,0],
           [4,0,3,5,7,8,9,6,0],
           [1,0,5,0,0,0,4,7,0],
           [0,8,0,1,0,4,0,3,5]]

solve :: Board -> Board
solve board = solve' board  
>>>>>>> 9342ef9c22ea98d934e24c4158eb30c72560b311

checkRow :: Board -> Int -> Int -> Bool
checkRow board row value = value `notElem` (board !! (row - 1))

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

<<<<<<< HEAD
checkRules :: Board -> Int -> Int -> Int -> Bool
checkRules board row column value = checkBox board row column value && checkColumn board column value && checkRow board row value 


=======
--Alternativ
{-
checkColumn1 :: Board -> Int -> Int -> Bool
checkColumn1 board column value = checkColumn' board 0 column value
    where 
        checkColumn' board 9 column value = True
        checkColumn' board row column value 
            | value == (board !! row !! column) = False
            | otherwise = checkColumn' board (row+1) column value 
-}
>>>>>>> 9342ef9c22ea98d934e24c4158eb30c72560b311
