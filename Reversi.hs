module Reversi where

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
-- data MoveRes = MoveRes GameBoard (Int, Int)

reversi :: Game
reversi move (State board (mine, opp, player, other)) 
 | win move board (mine, opp, player, other) = EndOfGame player reversi_start
 | newMine == 32 && newOpp == 32 = EndOfGame 't' reversi_start
 | otherwise = ContinueGame (State newBoard (newMine, newOpp, other, player))
    where (State newBoard (newMine, newOpp, other, player)) = updateBoard move (State board (mine, opp, player, other))


win :: Action -> Board -> (Int, Int, Char, Char) -> Bool
win (Action (row,col)) board (mine, opp, player, other)
 | (noMoreMoves player other board row col) && mine > opp = True
 | otherwise = False

noMoreMoves :: Char -> Char -> Board -> Int -> Int -> Bool
noMoreMoves player other board row col = 
    (checkPlay player other board row col (\ i -> i) (\ j -> j+1) (\ i -> i) (\j -> j-1)) || -- check horizontal
    (checkPlay player other board row col (\ i -> i+1) (\ j -> j) (\ i -> i-1) (\j -> j)) || -- check Vertical 
    (checkPlay player other board row col (\ i -> i-1) (\ j -> j-1) (\ i -> i+1) (\j -> j+1)) || -- check diagnal up
    (checkPlay player other board row col (\ i -> i+1) (\ j -> j-1) (\ i -> i-1) (\j -> j+1))  -- check diagnal down

checkPlay :: Char -> Char -> Board -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Bool
checkPlay player other board row col fRow fCol gRow gCol = (checkBoard player other board row col fRow fCol) || (checkBoard player other board row col gRow gCol)

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


{- Updating board-}
updateBoard :: Action -> State -> State
updateBoard (Action (r,c)) (State board (mine, opp, player, other)) = 
  let newBoard = updateAll row col board player other
      (newMine, newOpp) = countMine newBoard
  in (State newBoard (newOpp, newMine, other, player))



updateAll :: Int -> Int -> Board -> Char -> Char -> Board
updateAll row col board player other = 
  let rowToUpdate = board !! row
      updatedRow = switchColors col rowToUpdate player other
      colToUpdate = board !! col
      updatedCol = switchColors row colToUpdate player other
      newBoard = replaceNth row col rowToUpdate colToUpdate newBoard
      newBoardForReal = switchColorsDiagonal row col player other
  in (Board newBoardForReal)

switchColors :: Int -> [Char] -> Char -> Char
switchColors rc rcToUpdate player other
 | needsAction = flip



replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

{- Updating board-}

createBoardState :: Board -> [(Int, Int)] -> State
createBoardState board lst =
    let actions = [Action a |  a <- lst]
    in (State board actions)

reversi_start  = createBoardState [ 
--   a1    b2   c3   d4   e5   f6   g7   h8
1    ['*', '*', '*', '*', '*', '*', '*', '*'], 
 2   ['*', '*', '*', '*', '*', '*', '*', '*'],
 3   ['*', '*', '*', '*', '*', '*', '*', '*'],
 4   ['*', '*', '*', 'O', 'X', '*', '*', '*'],
 5   ['*', '*', '*', 'X', 'O', '*', '*', '*'],
  6  ['*', '*', '*', '*', '*', '*', '*', '*'],
 7   ['*', '*', '*', '*', '*', '*', '*', '*'],
  8  ['*', '*', '*', '*', '*', '*', '*', '*']]
 [(5,6), (6,5), ]
-- a i = Action i 

-- reversi 'X' (2,3) reversi_start




