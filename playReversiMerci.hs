module Play where
--module based on Play.hs from class
import ReversiMerci

import System.IO
import Text.Read   (readMaybe)

--to play
-- play reversi reversi_start 'X' (0,0,0)

get_ans s = 
    do
        putStr s
        line <- getLine
        case (readMaybe line :: Maybe Int) of
            Nothing ->
                get_ans s
            Just i ->
                return (read line)
    
get_num_ans :: (Read b, Num b) => String -> IO b
get_num_ans = get_ans

--switch_opp p1
--    |p1=='X' = 'O'
--    |otherwise = 'X'

col_position_arr = ['0','1','2','3','4','5','6','7']
position_arr = ['+','0','1','2','3','4','5','6','7']
number_rows board = [y:x | (x,y) <- (zip board position_arr)]
show_numbered_board :: [[Char]] -> IO ()
show_numbered_board board= 
    let num_board = number_rows (col_position_arr:board)
    in putStr (unlines [unwords [show (num_board !! y !! x) | x <- [0..8]] | y <- [0..8]])


type CurrentGameStatus = (Int,Int,Int)   -- wins, losses, ties
play :: Game -> State -> Player -> CurrentGameStatus -> IO CurrentGameStatus

play game start_state opponent ts =
  let (x_wins, o_wins,ties) = ts in
  do
      putStrLn ("Current Stats: "++ show x_wins++ " X wins "++show o_wins++" O wins "++show ties++" ties")
      putStrLn "What type of player? 0=person, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game (ContinueGame reversi_start) opponent ts "pvp"
        else if line ==  "1"
             then person_play game (ContinueGame reversi_start) opponent ts "pvcomp" 
        else if line == "2"
            then return ts
        else play game reversi_start opponent ts


person_play :: Game -> Result -> Player -> CurrentGameStatus -> [Char] -> IO CurrentGameStatus
-- opponent has played, the person must now play

person_play game (ContinueGame state) opponent ts opp_type =
   do
      let State board (int1,int2, char1, char2)  = state
      show_numbered_board board
      putStrLn ("current player: "++show char1)
      putStrLn ("score ("++show char1++","++show char2++"): ("++show int1++","++show int2++")")
      do 
        rowline <- get_num_ans "choose a row: "
        colline <- get_num_ans "choose a column: "
        if opp_type=="pvcomp"
            then computer_play game (game (Action (rowline, colline)) state) opponent ts opp_type  
        else person_play game (game (Action (rowline, colline)) state) opponent ts opp_type
        
person_play game (EndOfGame val start_state) opponent ts opp_type =
  do
    newts <- update_status val ts  
    play game start_state opponent newts

computer_play :: Game -> Result -> Player -> CurrentGameStatus -> [Char] -> IO CurrentGameStatus
computer_play game (EndOfGame val  start_state) opponent ts opp_type =
   do
      newts <- update_status val ts
      play game start_state opponent newts

computer_play game (ContinueGame state) opponent ts opp_type =
      let 
          State board (int1,int2, char1, char2)  = state
          next_move = (Action (choose_best state))
        in
            if (opp_type=="pvcomp") && (char1 /= 'O') -- make sure person played valid move and players have switched since it's person vs comp, computer is always second so it will be O
            then person_play game (ContinueGame state) opponent ts opp_type
            else 
            do
                putStrLn ("The computer chose "++show next_move)
                person_play game (game next_move state) opponent ts opp_type



--list all valid moves for current player
--list_valid_moves test_State => [(2,3),(3,2),(3,7),(4,6),(5,3),(5,4),(5,6)]
list_valid_moves :: State -> [(Int, Int)]
list_valid_moves (State board (int1,int2, char1, char2)) = [x | x <- (createCoordinate [0..7] [0..7]) , valid x board char1 char2]

choose_best_helper:: [(Int, Int)] -> State -> (Int, Int) -> Int -> (Int, Int)
choose_best_helper [] _ so_far _ = so_far
choose_best_helper (h:t) (State board (int1,int2, char1, char2)) best_coord best_val = 
    if new_int2 > best_val 
    then choose_best_helper t (State board (int1,int2, char1, char2)) h new_int2
    else choose_best_helper t (State board (int1,int2, char1, char2)) best_coord best_val
    where (State new_board (new_int1,new_int2, new_char1, new_char2)) = (updateBoard h (State board (int1,int2, char1, char2)))

--return best move for that state (for current player)
--choose_best test_State => (4,6)
choose_best :: State -> (Int, Int)
choose_best state = choose_best_helper (list_valid_moves state) state (-1,-1) 0

update_status:: Char -> CurrentGameStatus -> IO CurrentGameStatus
update_status val (x_wins,o_wins,ties)
  | val == 'X' = do
      putStrLn "X Won"
      return (x_wins+1, o_wins,ties)
  | val == 't' = do
      putStrLn "It's a tie"
      return (x_wins, o_wins,ties+1)
  | otherwise = do
      putStrLn "O won!"
      return (x_wins, o_wins+1,ties)
