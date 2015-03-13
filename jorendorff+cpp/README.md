# Microchess in C++

This directory contains a C++ port of Microchess,
a simple chess-playing program originally written for the
[KIM-1](http://www.6502.org/trainers/buildkim/kim.htm),
the first computer developed by Commodore.
The program is therefore in 6502 assembler.

Not exactly a functional programming language!
But I'll make up for that later.
In any case, all our chess-playing programs need something to play against.
So consider this a benchmark.


## How to play

    g++ -o microchess microchess.cpp
    ./microchess

The user interface is text-only.

*   Type <kbd>Q</kbd> and hit <kbd>Enter</kbd> to quit.

*   Enter <kbd>P</kbd> to tell the computer to take a turn.
    Microchess always plays white.

*   To take your turn,
    enter the coordinates of the piece you want to move,
    then the square you want to move it to,
    then <kbd>Enter</kbd>.
    For example:

        R N B K Q B N R   1
        P P P . P P P P   2
        . . . . . . . .   3
        . . . P . . . .   4
        . . . . . . . .   5
        . . . . . . . .   6
        p p p p p p p p   7
        r n b k q b n r   8

        h g f e d c b a        0F 13 33

    To counter with **e5**, you'd type <kbd>e7e5 Enter</kbd>.

    Then type <kbd>P</kbd> and Microchess will respond (with Nf3).
    And so on.

*   There's one more command, <kbd>R</kbd>, which reverses the board.
    Bizarrely, the letters and numbers around the edges stay where
    they are &mdash; the way this is implemented is a little weird.
    The UI ought to compensate, but it doesn&rsquo;t.


## How Microchess works

The board is represented by a 32-byte array called BOARD. Each byte is
location of one of the pieces or pawns in the game, or $CC if the piece
has been captured.

For example, if the byte at address $50 is $12, that means the white
king is at row 1, column 2. Microchess numbers the rows and columns
differently from standard chess notation&mdash;they&rsquo;re
zero-indexed and the columns are numbered in the opposite
direction&mdash;so this corresponds to square **c2**.

The main AI starts at NOOPEN. I haven't understood the whole thing.
Certainly there is a routine, GNM, that simply finds every possible move,
and calls another routine JANUS, for each one. JANUS can do
several different things; its behavior is determined by a STATE variable.
GNM is thus a utility that the AI uses in several different ways.

The AI certainly has the ability to look ahead at capture exchanges,
examining that part of the game tree depth-first. (This looking-ahead is
what JANUS does when STATE is between -5 and 0.) But that is only part
of the overall strategy.

Every chess AI needs a scoring algorithm, to estimate the desirability of
a possible future board state. Microchess has a fairly fancy one, but
again I'm fuzzy on the details. A few things are clear: it prefers board
states where white pieces have lots of available moves, and particularly
where the white queen is mobile. And of course it likes capturing
pieces.

One more thing. Microchess also conatins a script for one standard
opening.  If you let Microchess play white, it'll open with **e4**, and
if you play the black moves listed below, Microchess plays the white
moves.

1.  e4      e5      (<kbd>6343</kbd>)
2.  Nf3     Nc6     (<kbd>7655</kbd>)
3.  Bc4     Bc5     (<kbd>7245</kbd>)
4.  c3      Nf6     (<kbd>7152</kbd>)
5.  d4      exd4    (<kbd>4334</kbd>)
6.  cxd4    Bb4+    (<kbd>4536</kbd>)
7.  Nc3     Nxe4    (<kbd>5233</kbd>)
8.  o-o     Bxc3    (<kbd>3625</kbd>)
9.  bxc3

This is the only way Microchess can castle.
In fact, because Microchess doesn&rsquo;t really support castling,
after **8. o-o** the user has to move white's rook manually
by typing <kbd>h1f1 Enter</kbd>!


## Bugs in the original?

The source I started with came from
<http://benlo.com/files/Microchess6502.txt>
and [Microchess6502.txt](Microchess6502.txt) is a copy of that file.
However that code contains what seem like bugs
which I had to fix in the C++ port.

*   The initial value of the pointer SP2 is $C8, but that only leaves
    the move stack enough room to hold three moves. The fourth move
    starts overwriting the variables stored at $B0-$B7.

    Pretty sure this is a bug, since the game can look ahead at least 5
    moves (I think more like 8 but I'm not far enough along yet to be
    sure) and one of those variables is STATE which drives the whole
    algorithm.

*   The main routine CHESS does JSR GO to run the AI, but GO does not
    return to its caller with RTS. Instead it ends with JMP CHESS.  This
    should cause the stack to grow without bound as you play the game.

