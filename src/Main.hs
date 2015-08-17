module Main where

import System.IO
import System.Exit
import Control.Exception
import System.Console.Chess
import Chess

main = do
  putStrLn "Welcome to Console Chess!"
  putStrLn $ prettyBoard defaultBoard
  gameLoop defaultBoard

gameLoop board = do
  if (check White board)
    then putStrLn "White is checked!"
    else return ()
  if (check Black board)
    then putStrLn "Black is checked!"
    else return ()
  if (mate White board)
    then do
    putStrLn "Black has won. Congratulations!"
    exitWith ExitSuccess
    else return ()
  if (mate Black board)
    then do
    putStrLn "White has won. Congratulations!"
    exitWith ExitSuccess
    else return ()
  putStr "Your move: "
  hFlush stdout
  move <- getLine
  newBoard <- (evaluate $ moveSAN move board) `catch` (\(SomeException _) -> return $ Left NoParse)
  case newBoard of
   Left e -> do
     putStrLn "Invalid move, try again..."
     gameLoop board
   Right b -> do
     putStrLn $ prettyBoard b
     gameLoop b
