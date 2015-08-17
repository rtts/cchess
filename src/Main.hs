module Main where

import System.IO
import System.Console.Chess
import Chess

main = do
  putStrLn "Welcome to Console Chess!"
  putStrLn $ prettyBoard defaultBoard
  gameLoop defaultBoard

gameLoop board = do
  putStr "Your move: "
  hFlush stdout
  move <- getLine
  let newBoard = moveSAN move board
  case newBoard of
   Left _ -> do
     putStrLn "Invalid move, try again..."
     gameLoop board
   Right b -> do
     putStrLn $ prettyBoard b
     gameLoop b
