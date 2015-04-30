# Minimax implementation in Haskell

I know, everyone is supposed to do this in programming 101.
I never had that class.

[Minimax.hs](Minimax.hs) defines a `Game` typeclass.
If you have a type `g`, combined with a handful of other features,
what you get is a complete characterization of a game's rules.

```haskell
class Game g where
  type Move g
  start :: g
  moves :: g -> [Move g]
  applyMove :: g -> Move g -> g
  scoreFinishedGame :: g -> Float  -- returns >0 if last move won the game.
```

The type itself, **g**, has to be a data type that represents a game state.
Where all the pieces are on the board, whose turn it is -- all that stuff.

*   **Move g** is the type of moves in the game.
    For example, in Battleships, a move might be just a pair of coordinates,
    and so we'd have `type Move Battleships = (Int, Int)`.

*   **start** is an instance of type **g**, the game state
    just before the first turn of the game.

*   **moves** is a function.
    Feed it a game state and it returns a list of all the available moves.
    If it returns an empty list, that means the game is over.

*   **applyMove** is the function to call when someone takes their turn.
    Give it a game state and a move, and it computes the new game state.

*   And lastly **scoreFinishedGame** tells you who won.

It turns out that given this typeclass,
we can write very generic AI code that can then play *any game*
that conforms to the `Game` typeclass.

(A `Game` is a two-player game with perfect information. Assuming
infinite compute power, they all really can be played perfectly with the
same algorithm.)


## This program vs. ../jorendorff+cpp

*   The C++ one is a port of some 39-year-old assembly-language code.
*   The Haskell one is a weekend hack.
*   The C++ one is really fast
    but it's not clear from looking at the source code just what it is doing.
*   The core algorithm for the Haskell one is 5 lines of code
    *and it can also play Othello*.
*   The Haskell one is super slow, but it plays better chess.

If you have any more stereotypes about C++ and Haskell
that you would like confirmed, let me know.


## How to play

Want to play against the computer?

    cabal install parallel
    make play

If you happen to have [xboard](https://www.gnu.org/software/xboard/) installed,
do this instead:

    make playx

Or if you prefer a simpler game:

    make Reversi
    ./Reversi
