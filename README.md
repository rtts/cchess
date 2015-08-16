Console Chess
=============

<img src="http://rtts.eu/media/chessboard43.gif" width="65%" align="right">

Console Chess is a simple frontend for [UCI](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html)-compliant chess engines. To play a game of chess you need a console/terminal application that can display [Unicode chess symbols](https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode) and a [chess engine](http://computer-chess.org/doku.php?id=computer_chess:wiki:lists:chess_engine_list).

Installation
------------

Console Chess is written in programming language [Haskell](https://www.haskell.org/). You can compile the code by typing `cabal build` or `stack build`, then install it using `cabal install` or `stack install`.

Playing chess
-------------

After installation, the `cchess` command followed by the path to a chess engine will start a new game with the computer playing as black. Moves have to be entered at the prompt in [SAN notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)). Between each move the current board will be printed to the console.
