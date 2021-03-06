module Boards(boardEasy, boardMedium, boardHard, boardExpert, boardImpossible, boardEasyFinish) where 

import Logic

 {- boardEasy
     Example of an easy Sudoku board.
  -}
boardEasy :: Board
boardEasy = [[0,3,0,8,2,0,0,1,0],
           [0,6,1,4,5,7,8,2,3],
           [8,7,0,0,0,1,0,0,0],
           [2,4,8,0,0,3,0,5,7],
           [6,1,0,2,8,0,0,9,0],
           [0,0,0,7,4,0,0,8,0],
           [4,0,3,5,7,8,9,6,0],
           [1,0,5,0,0,0,4,7,0],
           [0,8,0,1,0,4,0,3,5]]

 {- boardMedium
     Example of a medium Sudoku board.
  -}
boardMedium :: Board
boardMedium = [
        [0, 1, 0, 0, 0, 0, 0, 0, 5],
        [2, 6, 0, 4, 3, 7, 0, 0, 0],
        [0, 0, 4, 0, 0, 0, 0, 0, 6],
        
        [0, 0, 0, 0, 5, 0, 7, 0, 9],
        [0, 0, 0, 7, 4, 3, 0, 0, 2],
        [0, 4, 0, 0, 0, 1, 0, 5, 0],
        
        [0, 0, 0, 0, 0, 6, 0, 9, 3],
        [9, 0, 0, 0, 0, 0, 8, 0, 7],
        [6, 3, 0, 1, 7, 9, 0, 0, 0]
    ]

{- boardHard
     Example of a hard Sudoku board.
  -}
boardHard :: Board
boardHard = [
           [0,0,8,1,5,0,9,0,2],
           [0,2,0,0,0,0,0,5,0],
           [0,0,0,0,2,0,1,6,4],
           [0,8,0,6,0,0,0,0,0],
           [0,0,3,4,0,8,2,0,0],
           [0,0,0,0,0,9,0,1,0],
           [3,5,9,0,8,0,0,0,0],
           [0,4,0,0,0,0,0,2,0],
           [2,0,6,0,9,4,3,0,0]]

{- boardExpert
     Example of a expert Sudoku board.
  -}
boardExpert :: Board
boardExpert = [
        [0, 0, 2, 0, 0, 8, 0, 0, 9],
        [7, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 4, 3, 0, 6, 0],
        
        [2, 0, 6, 0, 0, 0, 0, 0, 0],
        [0, 4, 0, 0, 1, 0, 7, 0, 0],
        [0, 0, 0, 9, 6, 0, 0, 0, 0],
        
        [9, 1, 0, 0, 0, 0, 0, 0, 7],
        [0, 8, 0, 0, 0, 0, 0, 5, 0],
        [4, 0, 0, 0, 8, 0, 9, 0, 0]
    ]

-- For testing
boardEasyFinish :: Board
boardEasyFinish = [
           [5,3,4,8,2,9,7,1,6],
           [9,6,1,4,5,7,8,2,3],
           [8,7,2,3,6,1,5,4,9],
           [2,4,8,9,1,3,6,5,7],
           [6,1,7,2,8,5,3,9,4],
           [3,5,9,7,4,6,1,8,2],
           [4,2,3,5,7,8,9,6,1],
           [1,9,5,6,3,2,4,7,8],
           [7,8,6,1,9,4,2,3,5]]

-- For testing
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

-- For testing
boardImpossible :: Board
boardImpossible = [
           [0,3,5,8,2,9,7,1,6],
           [9,6,1,4,5,7,8,2,3],
           [8,7,2,3,6,1,5,4,9],
           [2,4,8,9,1,3,6,5,7],
           [6,1,7,2,8,5,3,9,4],
           [3,5,9,7,4,6,1,8,2],
           [4,2,3,5,7,8,9,6,1],
           [1,9,5,6,3,2,4,7,8],
           [7,8,6,1,9,4,2,3,5]]
