{- List representing a sudoku board.
   9 Rows each containing 9 cells.
   INVARIANT: must have exactly 9 Rows
              all rows must have same length
 -}
type Grid = [Row] 

{- List representing a horisontal row in sudoku board.
   A row has 9 numbers.
   INVARIANT: length must be exactly 9
 -}
type Row = [Cell]

{- Represents a cell in a sudoku board
   List of possible numbers
   INVARIANT: numbers are 1-9
              a number can not appear twice
 -}
type Cell = [Int]


{-
-}
data SudokuValue = Int deriving (Read, Show, Enum, Eq, Ord)
type Board = [[SudokuValue]]

solve :: [[a]] -> [[a]]


{-main = do  
    putStrLn "Insert Soduko"  
    sudokuinput <- getLine  
    putStrLn (sudokuinput)-}

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
