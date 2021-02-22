module Play where

import ReversiMerci

import System.IO
import Text.Read   (readMaybe)

--to play
-- play reversi reversi_start 'X' (0,0,0)

prompt s = 
    do
        putStr s
        line <- getLine
        case (readMaybe line :: Maybe Int) of
            Nothing ->
                prompt s
            Just i ->
                return (read line)
    
numPrompt :: (Read b, Num b) => String -> IO b
numPrompt = prompt

col_position_arr = ['0','1','2','3','4','5','6','7']
position_arr = ['+','0','1','2','3','4','5','6','7']
number_rows board = [y:x | (x,y) <- (zip board position_arr)]
show_numbered_board board= 
    let num_board = number_rows (col_position_arr:board)
    in putStr (unlines [unwords [show (num_board !! y !! x) | x <- [0..8]] | y <- [0..8]])

--
--simple_player :: Player
--simple_player (State board (int1,int2, char1, char2)) = Action (2,4)

type CurrentGameStatus = (Int,Int,Int)   -- wins, losses, ties
play :: Game -> State -> Player -> CurrentGameStatus -> IO CurrentGameStatus

play game start_state opponent ts =
  let (x_wins, o_wins,ties) = ts in
  do
      putStrLn ("Current Stats: "++ show x_wins++ " X wins "++show o_wins++" O wins "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit, 3=skip."
      line <- getLine
      if line == "0"
        then
            pvp_play game (ContinueGame reversi_start) opponent ts
        else if line ==  "1"
             then computer_play game (ContinueGame reversi_start) opponent ts
        else if line == "2" 
            then return ts
        else play game reversi_start opponent ts

pvp_play :: Game -> Result -> Player -> CurrentGameStatus -> IO CurrentGameStatus
-- opponent has played, the person must now play

pvp_play game (ContinueGame state) opponent ts =
   do
      let State board (int1,int2, char1, char2)  = state
      show_numbered_board board
      putStrLn ("score: ("++show char1++","++show char2++") ("++show int1++","++show int2++")")
      do 
        rowline <- numPrompt "choose a row: "
        colline <- numPrompt "choose a column: "
        -- skip <- numPrompt "Press 3 to skip"
        
        if valid (rowline, colline) board char1 char2
          then
            computer_play game (game (Action (rowline, colline)) state) opponent ts
            else 
              do 
                putStrLn "press 3 to skip you turn"
                line <- getLine
                if line == "3"
                  then 
                    computer_play game (ContinueGame state) opponent ts 
                    else
                      pvp_play game (ContinueGame state) opponent ts


                    

pvp_play game (EndOfGame val start_state) opponent ts =
  do
    newts <- update_status val ts  
    play game start_state opponent newts

-- returnValidMove :: [(Int, Int)] -> Board -> Player -> Player -> (Int, Int)
computer_play game (ContinueGame state) opponent ts =
  do 
    let State board (mine, opp, player, other) = state
        (row,col) = returnValidMove (createCoordinate [0..7] [0..7]) board player other
      in
        do
          if row /= 8 && col /= 8
            then
              do
                putStrLn ("The computer chose "++show (row,col))
                pvp_play game (game (Action (row,col)) state) opponent ts
            else 
              pvp_play game (game (Action (row,col)) (State board (opp, mine, other, player))) opponent ts


computer_play game (EndOfGame val start_state) opponent ts = 
  do 
    newts <- update_status val ts  
    play game start_state opponent newts


    


update_status:: Char -> CurrentGameStatus -> IO CurrentGameStatus
update_status val (x_wins,o_wins,ties)
  | val == 'X' = do
      putStrLn "X Won"
      return (x_wins+1, o_wins,ties)
  | val == 't' = do
      putStrLn "It's a tie"
      return (x_wins, o_wins,ties+1)
  | otherwise = do
      -- putStrLn "O won!"
      putStrLn ("VALUE"++show val)
      return (x_wins, o_wins+1,ties)
