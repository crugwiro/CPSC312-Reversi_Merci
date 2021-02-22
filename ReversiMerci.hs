module ReversiMerci where

-- To run it, try:
-- ghci
-- :load MagicSum

data State = State Board (Int,Int, Char, Char)  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Char State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = Char


------ The Magic Sum Game -------

data Action = Action (Int, Int)                  -- a move for a player is just an Int
         deriving (Ord,Eq, Show)                 -- a move for a player is just an Int
type Board = [[Char]]  -- (self,other)
-- type InternalState = (Int,Int, Char, Char)




reversi :: Game 
reversi (Action (row,col)) (State board (mine, opp, player, other))
 | win board mine opp player other = EndOfGame player reversi_start
 | row == 8 && col == 8 = EndOfGame other reversi_start
 | boardFull board && mine == opp = EndOfGame 't' reversi_start 
 | otherwise = ContinueGame (updateBoard (row,col) (State board (mine, opp, player, other)))

 -- reversi (a (2,3)) test_end_State  
 --reversi (a (2,3)) reversi_start


-- updates the board
updateBoard :: (Int, Int) -> State -> State
updateBoard (row,col) (State board (mine, opp, player, other)) = 
    let preUpdateBoard = replaceNth1 row col player board 
        updatedBoard = updateBoardWithPlay preUpdateBoard row col player other
        (newMine, newOpp) = count updatedBoard
    in (State updatedBoard (newOpp, newMine, other, player))

count :: Board -> (Int,Int)
count board = 
    let 
        x_count = foldr (\x y -> (length(filter (\y -> y=='X') x)) + y) 0 board
        y_count = foldr (\x y -> (length(filter (\y -> y=='O') x)) + y) 0 board
    in (x_count, y_count)

updateBoardWithPlay :: Board -> Int -> Int -> Player -> Player -> Board
updateBoardWithPlay board row col player other = 
    let board1 = updateFollowUp board row col player other (\ i -> i) (\ j -> j+1)
        board2 = updateFollowUp board1 row col player other (\ i -> i) (\ j -> j-1)
        board3 = updateFollowUp board2 row col player other (\ i -> i + 1) (\ j -> j)
        board4 = updateFollowUp board3 row col player other (\ i -> i - 1) (\ j -> j)
        board5 = updateFollowUp board4 row col player other (\ i -> i + 1) (\ j -> j + 1)
        board6 = updateFollowUp board5 row col player other (\ i -> i - 1) (\ j -> j + 1)
        board7 = updateFollowUp board6 row col player other (\ i -> i + 1) (\ j -> j - 1)
        board8 = updateFollowUp board7 row col player other (\ i -> i - 1) (\ j -> j - 1)
    in (board8)




updateFollowUp :: Board -> Int -> Int -> Player -> Player -> (Int -> Int) -> (Int -> Int) -> Board
updateFollowUp board row col player other fRow fCol
 | (checkBoard player other board row col fRow fCol) = flipMerci board player (fRow row) (fCol col) fRow fCol -- horizontal right
 | otherwise = board

flipMerci :: Board -> Player -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Board
flipMerci board player row col fRow fCol
 -- | (isOutOfBound row col) || (board !! row !! col) == '.' || (board !! row !! col) == player = board
 | (isOutOfBound row col) || (board !! row !! col) == '*' || (board !! row !! col) == player = board
 | otherwise = flipMerci (replaceNth1 row col player board) player (fRow row) (fCol col) fRow fCol

replaceNth1 :: Int -> Int -> a -> [[a]] -> [[a]]
replaceNth1 _ _ _ [] = []
replaceNth1 row col newVal (x:xs)
 | row == 0 = replaceNth col newVal x : xs
 | otherwise = x:replaceNth1 (row-1) col newVal xs

-- replaces characters in board
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

-- a player wins if they have more points than the other player and neither plauer has any valid moves left
win :: Board -> Int -> Int -> Player -> Player -> Bool
win board mine opp player other
 | boardFull board && newMine > newOpp = True
 | noValidMoves board (createCoordinate [0..7] [0..7]) player other && noValidMoves board (createCoordinate [0..7] [0..7]) other player  && newMine > newOpp = True
 | otherwise = False
   where (newMine, newOpp) = count board
 -- win test_end_star 5 4 'X' 'O' 



-- checks whether a player has any valid moves available on the board
noValidMoves :: Board -> [(Int, Int)] -> Player -> Player -> Bool
noValidMoves _ [] _ _ = True
noValidMoves board (h:t) player other
 | valid h board player other = False
 | otherwise = noValidMoves board t player other

-- checks if a move is valid
valid :: (Int, Int) -> Board -> Player -> Player -> Bool
valid (row, col) board player other = 
 ((checkPlay player other board row col (\ i -> i) (\ j -> j+1) (\ i -> i) (\j -> j-1)) || -- check horizontal
 (checkPlay player other board row col (\ i -> i+1) (\ j -> j) (\ i -> i-1) (\j -> j)) || -- check Vertical 
 (checkPlay player other board row col (\ i -> i-1) (\ j -> j-1) (\ i -> i+1) (\j -> j+1)) || -- check diagnal up
 (checkPlay player other board row col (\ i -> i+1) (\ j -> j-1) (\ i -> i-1) (\j -> j+1))) &&  -- check diagnal down
 -- (board !! row !! col) == '.'
 (board !! row !! col) == '*'

-- helper for valid
checkPlay :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkPlay player other board row col fRow fCol gRow gCol = (checkBoard player other board row col fRow fCol) || (checkBoard player other board row col gRow gCol)

-- helper for checkPlay
checkBoard :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
checkBoard player other board row col fRow fCol
 | (isOutOfBound row col) || ((isOutOfBound (fRow row) (fCol col)) || player == (board !! (fRow row) !! (fCol col))) = False
 | ((board !! (fRow row) !! (fCol col)) == other) && not (isOutOfBound (fRow row) (fCol col)) = plaYable player board (fRow row) (fCol col) fRow fCol
 | otherwise = False


plaYable :: Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
plaYable player board row col fRow fCol
 | isOutOfBound row col = False
 -- | board !! row !! col == '.' = False
 | board !! row !! col == '*' = False
 | board !! row !! col == player = True
 | otherwise = plaYable player board (fRow row) (fCol col) fRow fCol


isOutOfBound :: Int -> Int -> Bool
isOutOfBound row col = row > 7 || col > 7 || row < 0 || col < 0
 

-- {check if board is full
boardFull :: Board -> Bool
boardFull [] = True
boardFull (h:t)
 | anyOpen h = False
 | otherwise = boardFull t

anyOpen ::[Char] -> Bool
anyOpen row = or [x == '*'| x <- row]
-- anyOpen row = or [x == '.'| x <- row]

returnValidMove :: [(Int, Int)] -> Board -> Player -> Player -> (Int, Int)
returnValidMove [] _ _ _ = (8,8)
returnValidMove (h:t) board player other
 | valid h board player other = h
 | otherwise = returnValidMove t board player other

printState (ContinueGame (State board (mine, opp, player, other))) = show_board board
printState1 (State board (mine, opp, player, other)) = show_board board


-- Simple Player ---
simple_player :: State -> Result
simple_player (State board (mine, opp, player, other))
 | noValidMoves board (createCoordinate [0..7] [0..7]) player other = ContinueGame (State board (opp, mine, other, player))
 | otherwise = reversi (Action (row, col)) (State board (mine, opp, player, other))
    where (row, col) = returnValidMove (createCoordinate [0..7] [0..7]) board player other
 

-- check if board is full
createCoordinate lst lst1 =  [(x,y)| x <- lst, y <- lst1]
-- createAllMoves = 
show_board board= putStr (unlines [unwords [show (board !! y !! x) | x <- [0..7]] | y <- [0..7]])

-- reversi_start = State [['*', '*', '*', '*', '*', '*', '*', '*'], 
--                        ['*', '*', '*', '*', '*', '*', '*', '*'],
--                        ['*', '*', '*', '*', '*', '*', '*', '*'],
--                        ['*', '*', '*', 'O', 'X', '*', '*', '*'],
--                        ['*', '*', '*', 'X', 'O', '*', '*', '*'],
--                        ['*', '*', '*', '*', '*', '*', '*', '*'],
--                        ['*', '*', '*', '*', '*', '*', '*', '*'],
--                        ['*', '*', '*', '*', '*', '*', '*', '*']] (0, 0, 'X', 'O')


reversi_start = State 
   [['*', '*', '*', '*', 'X', '*', '*', '*'], 
    ['*', '*', '*', '*', 'X', 'X', '*', '*'],
    ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'O'],
    ['X', 'X', 'X', 'X', 'X', 'X', '*', 'O'],
    ['*', 'X', 'X', 'O', '*', '*', '*', 'O'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*']] (16, 3, 'X', 'O')

-- reversi_start = 
-- --              0    1    2    3    4    5    6    7
-- {-0-}  State [['.', '.', '.', '.', 'O', '.', '.', '.'], 
-- {-1-}         ['.', '.', '.', '.', 'O', 'O', '.', '.'],
-- {-2-}         ['O', 'O', 'O', 'O', 'X', '.', 'O', 'X'],
-- {-3-}         ['.', '.', 'O', 'O', 'O', 'O', '.', 'X'],
-- {-4-}         ['.', '.', 'O', 'O', 'O', '.', '.', 'X'],
-- {-5-}         ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-6-}         ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-7-}         ['.', '.', '.', '.', '.', '.', '.', '.']] (0, 0, 'O', 'X')

 -- a i = Action i
-- reversi  (Action (5 5)) reversi_start

test_board = [['*', '*', '*', '*', '*', '*', '*', '*'], 
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', 'X', '*'],
    ['*', '*', '*', 'O', 'X', 'O', 'O', '*'],
    ['*', '*', '*', 'X', 'O', 'O', 'X', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*']]

test_State = State [['*', '*', '*', '*', '*', '*', '*', '*'], 
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', 'X', '*'],
    ['*', '*', '*', 'O', 'X', 'O', 'O', '*'],
    ['*', '*', '*', 'X', 'O', 'O', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*']] (3, 5, 'X', 'O')

-- a i = Action i
-- updateBoard (a (4,6)) test_State  
-- returns State ["********","********","******X*","***OXOX*","***XXXX*","********","********","********"] (2,7,'O','X')

test_end = 
--        0    1    2    3    4    5    6    7
{-0-}  [['.', '.', '.', '.', 'O', '.', '.', '.'], 
{-1-}   ['.', '.', '.', '.', 'O', 'O', '.', '.'],
{-2-}   ['O', 'O', 'O', 'O', 'O', 'O', 'O', 'X'],
{-3-}   ['.', '.', 'O', 'O', 'O', 'O', '.', 'X'],
{-4-}   ['.', '.', 'O', 'O', 'O', '.', '.', 'X'],
{-5-}   ['.', '.', '.', '.', '.', '.', '.', '.'],
{-6-}   ['.', '.', '.', '.', '.', '.', '.', '.'],
{-7-}   ['.', '.', '.', '.', '.', '.', '.', '.']]

-- reversi_start = 
-- --        0    1    2    3    4    5    6    7
-- {-0-}  State [['.', '.', '.', '.', 'O', '.', '.', '.'], 
-- {-1-}   ['.', '.', '.', '.', 'O', 'O', '.', '.'],
-- {-2-}   ['O', 'O', 'O', 'O', 'O', 'O', 'O', 'X'],
-- {-3-}   ['.', '.', 'O', 'O', 'O', 'O', '.', 'X'],
-- {-4-}   ['.', '.', 'O', 'O', 'O', '.', '.', 'X'],
-- {-5-}   ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-6-}   ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-7-}   ['.', '.', '.', '.', '.', '.', '.', '.']] (0, 0, 'X', 'O')

test_end_star = 
   [['*', '*', '*', '*', 'X', '*', '*', '*'], 
    ['*', '*', '*', '*', 'X', 'X', '*', '*'],
    ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'O'],
    ['*', '*', 'X', 'X', 'X', 'X', '*', 'O'],
    ['*', '*', 'X', 'X', 'X', '*', '*', 'O'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*']]

test_update = 
    State [['*', '*', '*', '*', 'X', '*', '*', '*'], 
           ['*', '*', '*', '*', 'O', 'X', '*', '*'],
           ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'O'],
           ['*', '*', 'X', 'X', 'X', 'X', '*', 'O'],
           ['*', '*', 'X', 'X', 'X', '*', '*', 'O'],
           ['*', '*', '*', '*', '*', '*', '*', '*'],
           ['*', '*', '*', '*', '*', '*', '*', '*'],
           ['*', '*', '*', '*', '*', '*', '*', '*']] (17, 3, 'O','X')

brd = 
   [['*', '*', '*', '*', 'X', '*', '*', '*'], 
    ['*', '*', '*', '*', 'X', 'X', '*', '*'],
    ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'O'],
    ['X', 'X', 'X', 'X', 'X', 'X', '*', 'O'],
    ['X', 'X', 'X', 'X', 'X', '*', '*', 'O'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*']]

test_end_State = 
   State [['*', '*', '*', '*', 'X', '*', '*', '*'], 
    ['*', '*', '*', '*', 'X', 'X', '*', '*'],
    ['X', 'X', 'X', 'X', 'X', 'X', 'X', 'O'],
    ['*', '*', 'X', 'X', 'X', 'X', '*', 'O'],
    ['*', '*', 'X', 'X', 'X', '*', '*', 'O'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*'],
    ['*', '*', '*', '*', '*', '*', '*', '*']] (17, 3, 'X','O')


-- to test this change 

