module ReversiMerci where

-- To run it, try:
-- ghci
-- :load MagicSum

data State = State Board InternalState  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Char State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Player -> Action -> State -> Result

type Player = Char


------ The Magic Sum Game -------

data Action = Action (Int, Int)                  -- a move for a player is just an Int
         deriving (Ord,Eq, Show)                 -- a move for a player is just an Int
type Board = [[Char]]  -- (self,other)
type InternalState = (Int,Int, Char, Char)




reversi :: Game 
reversi (Action row col) (State board (mine, opp, player, other))
 | win board mine opp player other = EndOfGame player reversi_start
 | boardFull board && mine == opp = EndOfGame 't' reversi_start
 | otherwise = ContinueGame (State newBoard (newOpp, newMine, other, player))
    where (State newBoard (newMine, newOpp, player, other)) = updateBoard move (State board (mine, opp, player, other))

-- updates the board
updateBoard :: Action -> State -> State
updateBoard (Action row col) (State board (mine, opp, player, other)) = 
	let rowToUpdate = board !! row
	    updatedRow = replaceNth col player rowToUpdate
	    preUpdateBoard = replaceNth row updatedRow board
	    updatedBoard = updateBoardWithPlay board row col player other
	    newMine = count player updatedBoard
	    newOpp = count other updatedBoard
	in (State updatedBoard (newOpp, newMine, other, player))

updateBoardWithPlay Board -> Int -> Int -> Player -> Player -> Board
updateBoardWithPlay board row col player other = 
    let board1 = updateFollowUp board row col player other (\ i -> i) (\ j -> j+1)
        board2 = updateFollowUp board1 row col player other (\ i -> i) (\ j -> j-1)
        board3 = updateFollowUp board2 row col player other (\ i -> i + 1) (\ j -> j)
        board4 = updateFollowUp board3 row col player other (\ i -> i - 1) (\ j -> j)
        board5 = updateFollowUp board4 row col player other (\ i -> i + 1) (\ j -> j + 1)
        board6 = updateFollowUp board5 row col player other (\ i -> i - 1) (\ j -> j + 1)
        board7 = updateFollowUp board6 row col player other (\ i -> i + 1) (\ j -> j - 1)
        board8 = updateFollowUp board7 row col player other (\ i -> i - 1) (\ j -> j - 1)
    in (Board board8)




updateFollowUp Board -> Int -> Int -> Player -> Player -> (Int -> Int) -> Board
updateFollowUp board row col player other fRow gRow
 | (checkBoard player other board row col fRow fCol) = flip board player row col fRow fCol -- horizontal right
 | otherwise = (Board board)

flip :: Board -> Player -> Int -> Int -> (Int -> Int) -> Board
flip board player row col fRow fCol
 | outOfBounds row col || board !! row !! col == '*' = (Board board)
 | otherwise = flip (replaceNth1 fRow fCol player board)

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
win :: Board -> Int -> Int -> Bool
win board mine opp player other
 | boardFull board && mine > opp = True
 | noValidMoves board boardSpaces player other && noValidMoves board boardSpaces player other && mine > opp = True
 | otherwise = False

-- checks whether a player has any valid moves available on the board
noValidMoves :: Board -> [(Int, Int)] -> Player -> Player -> Bool
noValidMoves board (h:t) player other
 | valid h board player other = False
 | otherwise = board player t

-- checks if a move is valid
valid :: (Int, Int) -> Board -> Player -> Player -> Bool
valid (row, col) board player other = 
	(checkPlay player other board row col (\ i -> i) (\ j -> j+1) (\ i -> i) (\j -> j-1)) || -- check horizontal
    (checkPlay player other board row col (\ i -> i+1) (\ j -> j) (\ i -> i-1) (\j -> j)) || -- check Vertical 
    (checkPlay player other board row col (\ i -> i-1) (\ j -> j-1) (\ i -> i+1) (\j -> j+1)) || -- check diagnal up
    (checkPlay player other board row col (\ i -> i+1) (\ j -> j-1) (\ i -> i-1) (\j -> j+1))  -- check diagnal down

-- helper for valid
checkPlay :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkPlay player other board row col fRow fCol gRow gCol = (checkBoard player other board row col fRow fCol) || (checkBoard player other board row col gRow gCol)

-- helper for checkPlay
checkBoard :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
checkBoard player other board row col fRow fCol
 | (outOfBounds row col) || player == (board !! (fRow row) !! (fCol col)) = False
 | (board !! (fRow row) !! (fCol col)) == other = plaYable player board (fRow row) (fCol col) fRow fCol
 | otherwise = False


plaYAble :: Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
plaYable player board row col fRow fCol
 | (outOfBounds row col) = False
 | (board !! row !! col ) == player = True
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

-- check if board is full}



