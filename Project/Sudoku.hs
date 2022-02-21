{-
-}
data SudokuValue = Int deriving (Read, Show, Enum, Eq, Ord)
type Grid = [[SudokuValue]]

{-main = do  
    putStrLn "Insert Soduko"  
    sudokuinput <- getLine  
    putStrLn (sudokuinput)-}
    
matrix :: Integer -> Integer -> ((Integer, Integer) -> Integer) -> t
matrix 4 4 $ \(i,j) -> 2*i - j

