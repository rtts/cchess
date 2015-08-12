data Piece = Piece Color Type deriving Eq
data Color = White | Black deriving Eq
data Type = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
data Board = Board [[Maybe Piece]] deriving Eq

initialBoard = Board $ map (map readPiece) boardSpec

boardSpec = [['r','n','b','q','k','b','n','r'],
             ['p','p','p','p','p','p','p','p'],
             [' ',' ',' ',' ',' ',' ',' ',' '],
             [' ',' ',' ',' ',' ',' ',' ',' '],
             [' ',' ',' ',' ',' ',' ',' ',' '],
             [' ',' ',' ',' ',' ',' ',' ',' '],
             ['P','P','P','P','P','P','P','P'],
             ['R','N','B','Q','K','B','N','R']]

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

data Square = Empty Color | Contains Piece Color deriving Eq
instance Show Square where
  show (Empty White) = "  "
  show (Empty Black) = invertString "  "
  show (Contains p White) = show p
  show (Contains p Black) = invertString . show . invertPiece $ p
invertPiece (Piece c t) = Piece (invertColor c) t
invertString s = "\27[7m" ++ s ++ "\27[27m"
invertColor Black = White
invertColor White = Black

instance Show Board where
  show (Board lines) = foldr (++) "a b c d e f g h" lineStrings
    where lineStrings = zipWith showLine lines [8,7..]
          showLine squares n = foldr (++) (show n ++ "\n") squareStrings
            where squareStrings = zipWith showSquare squares colors
                  showSquare (Just p) color = show (Contains p color)
                  showSquare Nothing color = show (Empty color)
                  colors = if even n then concat $ repeat [White, Black]
                           else concat $ repeat [Black, White]
