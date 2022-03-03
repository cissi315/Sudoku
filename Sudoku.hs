--import Gistfile1
--source = Gistfile1.main

module Sudoku(solve, insert, validMove) where 
import SudokuExample

type Board = [[Int]]

solve :: Board -> Board
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

insert :: Board -> Int -> Int -> Int -> Board
insert [] _ _ _ = []
insert board row column value = take (row - 1) board ++ insertAux (board !! (row-1)) column value : drop row board
    where insertAux :: [Int] -> Int -> Int -> [Int]
          insertAux [] _ _ = []
          insertAux (x:xs) c value -- vilken motsvarar row mm?
               | checkRules board row column value = take (c - 1) (x:xs) ++ value : drop c (x:xs)
               | otherwise = x:xs

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

checkRules :: Board -> Int -> Int -> Int -> Bool
checkRules board row column value = checkBox board row column value && checkColumn board column value && checkRow board row value 

{-
   EXAMPLES: find0 exampleFinish == (9,0)
 -}
find0 :: Board -> (Int, Int)
find0 board = (row board, column (board !! (row board - 1)))
    where
        row :: Board -> Int
        row [] = 0
        row board
            | 0 `elem` (board !! 0) = 1 
            | 0 `elem` (board !! 1) = 2
            | 0 `elem` (board !! 2) = 3
            | 0 `elem` (board !! 3) = 4
            | 0 `elem` (board !! 4) = 5
            | 0 `elem` (board !! 5) = 6
            | 0 `elem` (board !! 6) = 7
            | 0 `elem` (board !! 7) = 8
            | otherwise = 9

        column :: [Int] -> Int 
        column [] = 0
        column (x:xs) 
            | x == 0 = 9 - length xs
            | otherwise = column xs

validMove :: Board -> Int -> Int -> Int -> Bool
validMove board row column value
    | (board !! (row-1) !! (column-1)) /= 0 = False 
    | otherwise = validMove' (solve board) row column value
        where
            validMove' board row column value 
                | (board !! (row-1) !! (column-1)) == value = True 
                | otherwise = False

