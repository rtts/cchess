module System.Console.Chess (
  defaultBoard,
  prettyBoard
  )
where

import Chess
import Chess.FEN
import Data.Array

prettyBoard :: Board -> String
prettyBoard b = top ++ unlines lineStrings ++ bottm
  where lineStrings = zipWith prettyLine lines [8,7..]
        lines = [line n | n <- [7,6..0]]
        line n = [board b ! (x,n) | x <- [0..7]]

prettyLine :: [Maybe Piece] -> Int -> String
prettyLine squares n = left ++ concat squareStrings ++ right n
  where squareStrings = zipWith prettySquare squares colors
        colors = if even n then concat $ repeat [White, Black]
                 else concat $ repeat [Black, White]

prettySquare :: Maybe Piece -> Color -> String
prettySquare Nothing White = "  "
prettySquare Nothing Black = invertString "  "
prettySquare (Just p) White = prettyPiece p
prettySquare (Just p) Black = invertString . prettyPiece . invertPiece $ p
  where invertPiece (Piece c t) = Piece (invertColor c) t
        invertColor Black = White
        invertColor White = Black

prettyPiece (Piece White King)   = "♔ "
prettyPiece (Piece White Queen)  = "♕ "
prettyPiece (Piece White Rook)   = "♖ "
prettyPiece (Piece White Bishop) = "♗ "
prettyPiece (Piece White Knight) = "♘ "
prettyPiece (Piece White Pawn)   = "♙ "
prettyPiece (Piece Black King)   = "♚ "
prettyPiece (Piece Black Queen)  = "♛ "
prettyPiece (Piece Black Rook)   = "♜ "
prettyPiece (Piece Black Bishop) = "♝ "
prettyPiece (Piece Black Knight) = "♞ "
prettyPiece (Piece Black Pawn)   = "♟ "

top = "\n  ┌────────────────┐\n"
bottm = "  └────────────────┘\n   a b c d e f g h\n"
left = "  │"
right n = "│ " ++ show n

invertString :: String -> String
invertString s = "\27[7m" ++ s ++ "\27[27m"
