module Play where
--module based on Play.hs from class
import ReversiMerci

import System.IO
import Data.Time.Clock
import Text.Read   (readMaybe)

--to play
-- play reversi reversi_start 'X' (0,0,0)

--get user input and check that they've given an Int
get_ans :: Read b => String -> IO b
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


--positions to show on the board for to make choosing moves easier
col_position_arr = ['0','1','2','3','4','5','6','7']
position_arr = ['+','0','1','2','3','4','5','6','7']

--append the row number to each row in the board
number_rows :: [[Char]] -> [[Char]]
number_rows board = [y:x | (x,y) <- (zip board position_arr)]

--show board appending the row and column numbers
--using logic from 2019 connect 4 project logic and  https://stackoverflow.com/questions/18691321/printing-a-2d-array-in-haskell
show_numbered_board :: [[Char]] -> IO ()
show_numbered_board board= 
    let num_board = number_rows (col_position_arr:board)
    in putStr (unlines [unwords [show (num_board !! y !! x) | x <- [0..8]] | y <- [0..8]])

type CurrentGameStatus = (Int,Int,Int)   -- wins for X, wins for O, ties

--main function to start playing games
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
             then 
              do 
                putStrLn "Choose level: 0=Beginner, 1= Intermediate, 2 = Advanced"
                level <- getLine
                if level == "1"
                  then person_play game (ContinueGame reversi_start) opponent ts "pvcomp" 
                  else if level == "2"
                    then person_play game (ContinueGame reversi_start) opponent ts "pvcompH" 
                  else 
                    person_play game (ContinueGame reversi_start) opponent ts "pvcompB"
        else if line == "2"
            then return ts
        else play game reversi_start opponent ts


person_play :: Game -> Result -> Player -> CurrentGameStatus -> [Char] -> IO CurrentGameStatus
-- opponent has played, the person must now play
-- Includes a punishment-based timer which causes the player's turn to be skipped
-- if they have exceeded a certain amount of time when they make their move

person_play game (ContinueGame state) opponent ts opp_type =
   do
      let time = 30
      currTime <- getCurrentTime  --variable to instantiate original time
      let State board (int1,int2, char1, char2)  = state
      show_numbered_board board
      putStrLn ("current player: "++show char1)
      putStrLn ("score ("++show char1++","++show char2++"): ("++show int1++","++show int2++")")
      do 
        rowline <- get_num_ans "choose a row: "
        colline <- get_num_ans "choose a column: "
        currTimeNew <- getCurrentTime  
        if opp_type=="pvcomp" || opp_type == "pvcompH"
            then 
                if diffUTCTime currTimeNew currTime >=  time
                    then 
                    do 
                        putStrLn "You took too long to make your move..it is now your opponent's turn"
                        computer_play game (ContinueGame (State board (int2, int1, char2, char1))) opponent ts opp_type
                    else 
                    computer_play game (game (Action (rowline, colline)) state) opponent ts opp_type
            else if opp_type == "pvcompB"
                then  
                    if diffUTCTime currTimeNew currTime >=  time
                        then 
                        do 
                            putStrLn "You took too long to make your move..it is now your opponent's turn"
                            computer_playB game (ContinueGame (State board (int2, int1, char2, char1))) opponent ts opp_type 
                        else
                            computer_playB game (game (Action (rowline, colline)) state) opponent ts opp_type
            else 
                if diffUTCTime currTimeNew currTime >=  time 
                    then 
                    do 
                        putStrLn "You took too long to make your move..it is now your opponent's turn"
                        person_play game (ContinueGame (State board (int2, int1, char2, char1))) opponent ts opp_type
                    else 
                        person_play game (game (Action (rowline, colline)) state) opponent ts opp_type
        
person_play game (EndOfGame val start_state) opponent ts opp_type =
  do
    newts <- update_status val ts  
    play game start_state opponent newts


computer_play :: Game -> Result -> Player -> CurrentGameStatus -> [Char] -> IO CurrentGameStatus
computer_play game (EndOfGame val  start_state) opponent ts opp_type =
   do
      newts <- update_status val ts
      play game start_state opponent newts

-- this computer_play plays the intermediate or advanced version of the computer's play
-- based on the opp_type
computer_play game (ContinueGame state) opponent ts opp_type =
      let 
          State board (int1,int2, char1, char2)  = state
          next_move = (Action (choose_best state opp_type))
        in
            if (opp_type=="pvcomp") && (char1 /= 'O') -- make sure person played valid move and players have switched since it's person vs comp, computer is always second so it will be O
                then person_play game (ContinueGame state) opponent ts opp_type
            else if (opp_type=="pvcompH") && (char1 /= 'O')
                then person_play game (ContinueGame state) opponent ts opp_type
            else
            do
                putStrLn ("The computer chose "++show next_move)
                person_play game (game next_move state) opponent ts opp_type



--computer_playB is the easiest computer player, it chooses a valid move randomly
computer_playB :: Game -> Result -> Player -> CurrentGameStatus -> [Char] -> IO CurrentGameStatus
computer_playB game (EndOfGame val  start_state) opponent ts opp_type =
   do
      newts <- update_status val ts
      play game start_state opponent newts

--this is when the computer plays the beginner version of difficulty
computer_playB game (ContinueGame state) opponent ts opp_type =
      let 
          State board (int1,int2, char1, char2)  = state
          next_moveB = (Action (returnValidMove (createCoordinate [0..7] [0..7]) board char1 char2))
        in
            if (opp_type=="pvcompB") && (char1 /= 'O') -- make sure person played valid move and players have switched since it's person vs comp, computer is always second so it will be O
            then person_play game (ContinueGame state) opponent ts opp_type
            else 
            do
                putStrLn ("The computer chose "++show next_moveB)
                person_play game (game next_moveB state) opponent ts opp_type


--list all valid moves for current player
--list_valid_moves test_State => [(2,3),(3,2),(3,7),(4,6),(5,3),(5,4),(5,6)]
list_valid_moves :: State -> [(Int, Int)]
list_valid_moves (State board (int1,int2, char1, char2)) = [x | x <- (createCoordinate [0..7] [0..7]) , valid x board char1 char2]


-- chooses next best coordinates for intermediate person vs. computer game
choose_best_helper:: [(Int, Int)] -> State -> (Int, Int) -> Int -> (Int, Int)
choose_best_helper [] _ so_far _ = so_far
choose_best_helper (h:t) (State board (int1,int2, char1, char2)) best_coord best_val = 
    if new_int2 > best_val 
    then choose_best_helper t (State board (int1,int2, char1, char2)) h new_int2
    else choose_best_helper t (State board (int1,int2, char1, char2)) best_coord best_val
    where (State new_board (new_int1,new_int2, new_char1, new_char2)) = (updateBoard h (State board (int1,int2, char1, char2)))

--takes list of valid moves for current state and the current state 
--return list of tuples (move, state after opponent makes best move given your move)
look_ahead :: [(Int, Int)] -> State -> [((Int, Int), State)]
look_ahead [] _ = []
look_ahead (h:t) state = 
    let new_state = updateBoard h state
    in (h, (updateBoard (choose_best_helper (list_valid_moves new_state) new_state (8,8) 0) new_state)):(look_ahead t state)

-- given the list of moves and associated states from look_ahead, return the best move that will result in the current player having the highest score possible for them on their next move
best_look_ahead :: [(t, State)] -> t -> Int -> t
best_look_ahead [] so_far _= so_far
best_look_ahead ((coord, state):t) best_coord best_val =
--state is currently back to your next move after opponent made their best move given your current coord
    let (State new_board (new_int1,new_int2, new_char1, new_char2)) = updateBoard (choose_best_helper (list_valid_moves state) state (8,8) 0) state
    in if new_int2 > best_val
        then best_look_ahead t coord new_int2 
        else best_look_ahead t best_coord best_val


--return best move for that state (for current player) or for next state in the
-- case of advanced version: depends on opp_type
--choose_best test_State => (4,6)
choose_best :: State -> [Char]  -> (Int, Int)
choose_best state opp_type = 
    if opp_type == "pvcompH"
        then best_look_ahead (look_ahead (list_valid_moves state) state) (8,8) 0 
    else  
        choose_best_helper (list_valid_moves state) state (8,8) 0

--update the current game stats and show who won the current game
update_status:: Char -> CurrentGameStatus -> IO CurrentGameStatus
update_status val (x_wins,o_wins,ties)
  | val == 'X' = do
      putStrLn "X Won"
      putStrLn "Merci for the Reversi"
      return (x_wins+1, o_wins,ties)
  | val == 't' = do
      putStrLn "It's a tie"
      putStrLn "Merci for the Reversi"
      return (x_wins, o_wins,ties+1)
  | otherwise = do
      putStrLn "O won!"
      putStrLn "Merci for the Reversi"
      return (x_wins, o_wins+1,ties)



            