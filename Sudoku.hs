--ata SudokuValue = Int deriving (Read, Show, Enum, Eq, Ord)
type Board = [[Int]]

boardempty :: [[Integer]]
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
example = [[0,0,0,0,0,0,0,0,0],
           [0,4,6,1,8,2,5,9,3],
           [3,2,0,9,5,6,4,8,7],
           [7,3,5,4,0,1,9,6,0],
           [0,6,1,0,9,8,7,3,4],
           [4,0,9,5,3,7,8,1,2],
           [2,9,8,0,7,4,6,5,1],
           [1,5,4,0,0,9,2,7,8],
           [8,7,0,6,2,5,3,4,9]]
--solve ::

checkRow :: Board -> Int -> Int -> Bool
checkRow board row value = value `elem` (board !! row)

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
checkColumn grid column value = value `elem` checkColumn' grid column 
   where 
      checkColumn' [] _ = []
      checkColumn' (x:xs) column = (x !! (column-1)) : checkColumn' xs column

{-main = do  
    putStrLn "Insert Soduko"  
    sudokuinput <- getLine  
    putStrLn (sudokuinput)-}