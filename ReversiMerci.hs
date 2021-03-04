module ReversiMerci where

data State = State Board (Int,Int, Char, Char) 
         deriving (Ord, Eq, Show)

data Result = EndOfGame Char State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = Char

data Action = Action (Int, Int)                  -- a move for a player is just an Int
         deriving (Ord,Eq, Show)                 -- a move for a player is just an Int
type Board = [[Char]]  


--return the winner of the game given the characters and scores from the state
check_winner s1 s2 p1 p2 
    |s1 == s2 = 't'
    |s1 > s2 = p1
    |otherwise = p2


-- main game
-- takes a move and a state and updates the state with the action.
-- checks for the winning move
reversi :: Game 
reversi (Action (row,col)) (State board (mine, opp, player, other))
 | win board mine opp player other = EndOfGame (check_winner mine opp player other) reversi_start
 | row == 8 && col == 8 = EndOfGame (check_winner mine opp player other) reversi_start
 | valid (row,col) board player other = 
    let State new_board (int1,int2, char1, char2) = (updateBoard (row,col) (State board (mine, opp, player, other))) 
    in if win new_board int1 int2 char1 char2 
        then EndOfGame (check_winner mine opp player other) reversi_start 
        else if (noValidMoves new_board (createCoordinate [0..7] [0..7]) char1 char2) -- if the next player has no more moves continue with current
        then ContinueGame (State new_board (int2, int1, char2, char1))
        else ContinueGame (State new_board (int1,int2, char1, char2))
 | otherwise = ContinueGame (State board (mine, opp, player, other))

-- updates the board with a play
-- takes a (row, col) and the state, and updates the board with the current player's move
updateBoard :: (Int, Int) -> State -> State
updateBoard (row,col) (State board (mine, opp, player, other)) = 
    let preUpdateBoard = replaceNth1 row col player board 
        updatedBoard = updateBoardWithPlay preUpdateBoard row col player other
        (newMine, newOpp) = count updatedBoard
    in 
        if (player == 'X') 
        then
            (State updatedBoard (newOpp, newMine, other, player))
        else 
            (State updatedBoard (newMine, newOpp, other, player))

--count number of X and Y values currently on the board
count :: Board -> (Int,Int)
count board = 
    let 
        x_count = foldr (\x y -> (length(filter (\y -> y=='X') x)) + y) 0 board
        y_count = foldr (\x y -> (length(filter (\y -> y=='O') x)) + y) 0 board
    in (x_count, y_count)

-- check the board in all directions from (row, col) and update the board in all directions
-- takes a board, row, col and updates the board by flipping the opponent (other) with player chips
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

-- Helper for updateBoardwithPlay
updateFollowUp :: Board -> Int -> Int -> Player -> Player -> (Int -> Int) -> (Int -> Int) -> Board
updateFollowUp board row col player other fRow fCol
 | (checkBoard player other board row col fRow fCol) = flipMerci board player (fRow row) (fCol col) fRow fCol -- horizontal right
 | otherwise = board

-- flip opponents chips after valid play in a given direction
flipMerci :: Board -> Player -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Board
flipMerci board player row col fRow fCol
 | (isOutOfBound row col) || (board !! row !! col) == '.' || (board !! row !! col) == player = board
 | otherwise = flipMerci (replaceNth1 row col player board) player (fRow row) (fCol col) fRow fCol


-- replace a character in a 2D array
-- calls replaceNth to update specific rows/cols
replaceNth1 :: Int -> Int -> a -> [[a]] -> [[a]]
replaceNth1 _ _ _ [] = []
replaceNth1 row col newVal (x:xs)
 | row == 0 = replaceNth col newVal x : xs
 | otherwise = x:replaceNth1 (row-1) col newVal xs

-- replaces the character at position n with newVal
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

-- a player wins if they have more points than the other player and neither plauer has any valid moves left
win :: Board -> Int -> Int -> Player -> Player -> Bool
win board mine opp player other
 | boardFull board && newMine > newOpp = True
 | noValidMoves board (createCoordinate [0..7] [0..7]) player other && noValidMoves board (createCoordinate [0..7] [0..7]) other player = True
 | otherwise = False
   where (newMine, newOpp) = count board

-- checks whether a player has any valid moves available on the board
noValidMoves :: Board -> [(Int, Int)] -> Player -> Player -> Bool
noValidMoves _ [] _ _ = True
noValidMoves board (h:t) player other
 | valid h board player other = False
 | otherwise = noValidMoves board t player other

-- checks if a move is valid
-- checks if a player's move will flip at least one of the opp's player schip
-- checks if there is a valid play in all directions from given (row, col) position
valid :: (Int, Int) -> Board -> Player -> Player -> Bool
valid (row, col) board player other = 
 ((checkPlay player other board row col (\ i -> i) (\ j -> j+1) (\ i -> i) (\j -> j-1)) || -- check horizontal
 (checkPlay player other board row col (\ i -> i+1) (\ j -> j) (\ i -> i-1) (\j -> j)) || -- check Vertical 
 (checkPlay player other board row col (\ i -> i-1) (\ j -> j-1) (\ i -> i+1) (\j -> j+1)) || -- check diagnal up
 (checkPlay player other board row col (\ i -> i+1) (\ j -> j-1) (\ i -> i-1) (\j -> j+1))) &&  -- check diagnal down
 (board !! row !! col) == '.'

-- helper for valid
checkPlay :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkPlay player other board row col fRow fCol gRow gCol = (checkBoard player other board row col fRow fCol) || (checkBoard player other board row col gRow gCol)

-- helper for checkPlay
checkBoard :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
checkBoard player other board row col fRow fCol
 | (isOutOfBound row col) || ((isOutOfBound (fRow row) (fCol col)) || player == (board !! (fRow row) !! (fCol col))) = False
 | ((board !! (fRow row) !! (fCol col)) == other) && not (isOutOfBound (fRow row) (fCol col)) = plaYable player board (fRow row) (fCol col) fRow fCol
 | otherwise = False

-- Playable checks if in a given direction a play is valid
-- Returns true or false is a play is possible
plaYable :: Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
plaYable player board row col fRow fCol
 | isOutOfBound row col = False
 | board !! row !! col == '.' = False
 | board !! row !! col == player = True
 | otherwise = plaYable player board (fRow row) (fCol col) fRow fCol

--check if row and column are within bounds of board
isOutOfBound :: Int -> Int -> Bool
isOutOfBound row col = row > 7 || col > 7 || row < 0 || col < 0
 
-- {check if board is full
boardFull :: Board -> Bool
boardFull [] = True
boardFull (h:t)
 | anyOpen h = False
 | otherwise = boardFull t

--check if any open space in row
-- helper for checking if board is full
anyOpen ::[Char] -> Bool
anyOpen row = or [x == '.'| x <- row]


--takes a list of all coordinates and returns a vaid move
returnValidMove :: [(Int, Int)] -> Board -> Player -> Player -> (Int, Int)
returnValidMove [] _ _ _ = (8,8)
returnValidMove (h:t) board player other
 | valid h board player other = h
 | otherwise = returnValidMove t board player other

 
-- create all moves 
createCoordinate lst lst1 =  [(x,y)| x <- lst, y <- lst1]

reversi_start = State starting_board (2, 2, 'X', 'O')
                        
starting_board = [['.', '.', '.', '.', '.', '.', '.', '.'], 
                ['.', '.', '.', '.', '.', '.', '.', '.'],
                ['.', '.', '.', '.', '.', '.', '.', '.'],
                ['.', '.', '.', 'O', 'X', '.', '.', '.'],
                ['.', '.', '.', 'X', 'O', '.', '.', '.'],
                ['.', '.', '.', '.', '.', '.', '.', '.'],
                ['.', '.', '.', '.', '.', '.', '.', '.'],
                ['.', '.', '.', '.', '.', '.', '.', '.']]

-- test winning state
-- reversi_start = 
-- --              0    1    2    3    4    5    6    7
-- {-0-}  State [['.', '.', '.', '.', 'O', '.', '.', '.'], 
-- {-1-}         ['.', '.', '.', '.', 'O', 'O', '.', '.'],
-- {-2-}         ['O', 'O', 'O', 'O', 'X', '.', 'O', 'X'],
-- {-3-}         ['.', '.', 'O', 'O', 'O', 'O', '.', 'X'],
-- {-4-}         ['.', '.', 'O', 'O', 'O', '.', '.', 'X'],
-- {-5-}         ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-6-}         ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-7-}         ['.', '.', '.', '.', '.', '.', '.', '.']] (15, 4, 'O', 'X')

-- -- test no moves valid moves for opponent left
-- reversi_start = 
-- --              0    1    2    3    4    5    6    7
-- {-0-}  State [['.', '.', '.', '.', 'O', '.', 'O', '.'], 
-- {-1-}         ['.', '.', '.', '.', 'O', 'O', '.', '.'],
-- {-2-}         ['O', 'O', 'O', 'O', 'O', '.', 'O', 'X'],
-- {-3-}         ['.', '.', 'O', 'O', 'O', 'O', '.', 'X'],
-- {-4-}         ['.', '.', 'O', 'O', 'O', '.', '.', 'X'],
-- {-5-}         ['.', 'X', '.', '.', '.', '.', '.', '.'],
-- {-6-}         ['.', '.', '.', '.', '.', '.', '.', '.'],
-- {-7-}         ['.', '.', '.', '.', '.', '.', '.', '.']] (4, 16, 'X', 'O')




