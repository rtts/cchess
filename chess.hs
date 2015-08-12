data Piece = Piece Color Type deriving Eq
data Color = White | Black deriving Eq
data Type = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
data Board = Board [[Maybe Piece]] deriving Eq

instance Show Piece where
  show (Piece White King)   = "♔ "
  show (Piece White Queen)  = "♕ "
  show (Piece White Rook)   = "♖ "
  show (Piece White Bishop) = "♗ "
  show (Piece White Knight) = "♘ "
  show (Piece White Pawn)   = "♙ "
  show (Piece Black King)   = "♚ "
  show (Piece Black Queen)  = "♛ "
  show (Piece Black Rook)   = "♜ "
  show (Piece Black Bishop) = "♝ "
  show (Piece Black Knight) = "♞ "
  show (Piece Black Pawn)   = "♟ "

instance Show Board where
  show = showBoard

showBoard :: Board -> String
showBoard (Board lines) = foldr (++) "a b c d e f g h" lineStrings
  where lineStrings = zipWith showLine lines [8,7..]

showLine :: [Maybe Piece] -> Int -> String
showLine squares n = foldr (++) (show n ++ "\n") squareStrings
    where squareStrings = zipWith showSquare squares colors
          colors = if even n then concat $ repeat [White, Black]
                   else concat $ repeat [Black, White]

showSquare :: Maybe Piece -> Color -> String
showSquare Nothing White = "  "
showSquare Nothing Black = invertString "  "
showSquare (Just p) White = show p
showSquare (Just p) Black = invertString . show . invertPiece $ p
  where invertPiece (Piece c t) = Piece (invertColor c) t
        invertColor Black = White
        invertColor White = Black

invertString :: String -> String
invertString s = "\27[7m" ++ s ++ "\27[27m"

initialBoard = Board $ map (map readPiece)
               [['r','n','b','q','k','b','n','r'],
                ['p','p','p','p','p','p','p','p'],
                [' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' ',' '],
                ['P','P','P','P','P','P','P','P'],
                ['R','N','B','Q','K','B','N','R']]

readPiece :: Char -> Maybe Piece
readPiece ' ' = Nothing
readPiece 'r' = Just (Piece Black Rook)
readPiece 'n' = Just (Piece Black Knight)
readPiece 'b' = Just (Piece Black Bishop)
readPiece 'q' = Just (Piece Black Queen)
readPiece 'k' = Just (Piece Black King)
readPiece 'p' = Just (Piece Black Pawn)
readPiece 'R' = Just (Piece White Rook)
readPiece 'N' = Just (Piece White Knight)
readPiece 'B' = Just (Piece White Bishop)
readPiece 'Q' = Just (Piece White Queen)
readPiece 'K' = Just (Piece White King)
readPiece 'P' = Just (Piece White Pawn)
