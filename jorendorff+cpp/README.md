# MicroChess in C++

This directory contains a C++ port of
[MicroChess](https://chessprogramming.wikispaces.com/MicroChess),
a simple chess-playing program by Peter Jennings,
originally written for the
[KIM-1](http://www.6502.org/trainers/buildkim/kim.htm),
the first computer developed by Commodore.
The program is therefore in 6502 assembler.

Not exactly a functional programming language!
But I&rsquo;ll make up for that later.
In any case, all our chess-playing programs need something to play against.
So consider this a benchmark.

&ldquo;As can be seen, MICROCHESS is only the beginning&rdquo;  
&mdash;Peter Jennings, the MicroChess Manual



## How to play

    g++ -o microchess microchess.cpp
    ./microchess

I wrote the user interface.
It&rsquo;s text-only, but if you think that&rsquo;s primitive, consider that
MicroChess in its original form communicated through seven-segment displays.

*   Type <kbd>Q</kbd> and hit <kbd>Enter</kbd> to quit.

*   Enter <kbd>P</kbd> to tell the computer to take a turn.
    MicroChess always plays white.

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

    To counter with **e5**, you&rsquo;d type <kbd>e7e5 Enter</kbd>.

    Then type <kbd>P</kbd> and MicroChess will respond (with Nf3).
    And so on.

*   There&rsquo;s one more command, <kbd>R</kbd>, which reverses the
    board.  Bizarrely, the letters and numbers around the edges stay
    where they are &mdash; the way this is implemented is a little
    weird.  It actually reverses the positions of all the pieces in
    memory.  (The UI maybe ought to compensate, but it doesn&rsquo;t.)

MicroChess peculiarities:

*   MicroChess doesn&rsquo;t enforce any rules whatsoever.
    You can move your pieces in illegal ways.
    You can take six turns in a row.
    You can move the computer&rsquo;s pieces.

*   MicroChess does not support castling, *en passant*, or promotion.

    You can castle anyway, simply by moving your king and rook to the
    appropriate squares, and capture *en passant* by moving your pawn
    first on top of the opposing pawn and then to the square where it
    should go. There is no way to promote, though.



## How MicroChess works

MicroChess originally sold on cassette tape for $5,
and for that price you got not only the software but a
[37-page manual](http://archive.computerhistory.org/projects/chess/related_materials/text/4-1.MicroChess_%20Manual_for_6502.Micro-Ware/MicroChessManual.PETER_JENNINGS.062303071.sm.pdf)
containing not only instructions on how to play
but a fairly technical explanation of the algorithm.

I didn&rsquo;t read it all, so what I know is mainly from looking at the code.
Here are the basics.

The board is represented by a 32-byte array called BOARD. Each byte is
location of one of the pieces or pawns in the game, or $CC if the piece
has been captured.

For example, if the byte at address $50 is $12, that means the white
king is at row 1, column 2. MicroChess numbers the rows and columns
differently from standard chess notation&mdash;they&rsquo;re
zero-indexed and the columns are numbered in the opposite
direction&mdash;so this corresponds to square **c2**.

The main AI starts at NOOPEN. It&rsquo;s rather bewildering; here are the
parts that make sense to me:

*   There is a routine, GNM, that simply finds every possible move,
    and calls another routine JANUS, for each one.

    In other words, it&rsquo;s `map JANUS availableMoves`.

    JANUS can do several different things; its behavior is determined by
    the variable STATE. GNM is thus a utility that the AI uses in
    several different ways. The poor man&rsquo;s higher-order function.

*   The AI has the ability to look ahead at capture exchanges, examining
    that part of the game tree depth-first. (This looking-ahead is what
    JANUS does when STATE is between -5 and 0.)  From the manual:

    > **Exchange counts** are used to analyse the effect of the
    > potential exchange combinations. Each count reflects the maximum
    > number of points capturable at each level of an exchange
    > combination. Capture chains are halted by pawn captures, king
    > captures, or by reaching a limit of three captures per
    > side.

*   Every chess AI needs a scoring algorithm, to compare the possible
    moves and decide which is best. MicroChess has a fancy one. It likes
    for its pieces to have lots of available moves, and particularly for
    the queen to be mobile. It values attacking its opponent&rsquo;s
    high-value pieces. And it values attacking a large total value of
    material (it will totally fork your pieces with a knight given the
    chance).

MicroChess does *not* contain a general minimax algorithm. From the
manual:

> With the exception of the capture tree, the MICROCHESS program
> analyses  in  full only one move for each side beyond the move
> it will make.  It  is  possible  to  use  the  same  recursive
> technique  used  by  TREE  to  carry  out a full analysis to a
> further depth.  To do this would require a routine to  analyse
> and evaluate each intermediate position arrived at.  Sequences
> of  possible positions with positive values for computer moves
> and negative values for opponent's moves can be summed to give
> the total long term value of each  currently  available  move.

Even this is not a description of minimax, as it contemplates move
values being "summed" rather than the one best move being selected.

> In  order to be time efficient, this analysis can be performed
> on a subset of the available continuations selected by a quick
> static analysis.  In addition,  a  system  of  'tree  pruning'
> should  be  implemented  to  prevent  long excursions down low
> valued  branches.   Programmers  embarking  on  this  type  of
> program should bear in mind that from an average position with
> 50  available  moves  per  side,  a  total  of  15.625 billion
> sequences are generated in three moves per side.

(It&rsquo;s true: 50<sup>6</sup> is 15.625 billion;
but the average number of moves available to the player
in any given position is more like 35, and 35<sup>6</sup>
isn&rsquo;t even 2 billion! So you see.)

**Opening.** One more cool thing. MicroChess also conatins a script for
one standard opening.  If you let MicroChess play white, it&rsquo;ll open with
**e4**, and if you play the black moves listed below, MicroChess plays
the white moves.

1.  e4      e5      (<kbd>e7e5</kbd>)
2.  Nf3     Nc6     (<kbd>b8c6</kbd>)
3.  Bc4     Bc5     (<kbd>f8c5</kbd>)
4.  c3      Nf6     (<kbd>g8f6</kbd>)
5.  d4      exd4    (<kbd>e5d4</kbd>)
6.  cxd4    Bb4+    (<kbd>c5b4</kbd>)
7.  Nc3     Nxe4    (<kbd>f6e4</kbd>)
8.  o-o     Bxc3    (<kbd>b4c3</kbd>)
9.  bxc3

This is the only way MicroChess can castle.
In fact, because MicroChess doesn&rsquo;t really support castling,
after **8. o-o** the user has to move white&rsquo;s rook manually
by typing <kbd>h1f1 Enter</kbd>!

The whole opening is encoded in 28 bytes, and the manual explains
how to replace this opening with four others.


## Source

The source I started with came from <http://benlo.com/files/Microchess6502.txt>
and [Microchess6502.txt](Microchess6502.txt) is a copy of that file.

The [MicroChess manual](http://archive.computerhistory.org/projects/chess/related_materials/text/4-1.MicroChess_%20Manual_for_6502.Micro-Ware/MicroChessManual.PETER_JENNINGS.062303071.sm.pdf)
contains an earlier listing.
