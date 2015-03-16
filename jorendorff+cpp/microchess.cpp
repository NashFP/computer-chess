/************************************************************************
 *
 *  Kim-1 MicroChess (c) 1976-2005 Peter Jennings, www.benlo.com
 *
 ************************************************************************
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *
 * modified by Daryl Rictor to work over a
 * serial terminal connection, August 2002.
 *
 * Updated with corrections to earlier OCR errors by Bill Forster, August 2005.
 *
 * Translated to C++ and heavily rearranged by Jason Orendorff, March 2015.
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <ctype.h>

/*** Board representation ****************************************************/

#define CAPTURED ((uint8_t) 0xcc)

static uint8_t board[32];
#define bk (board + 16)

/*
 * Initial positions of all the pieces.
 */
static const uint8_t setw[32] = {
    // White pieces: K Q R R B B N N
    0x03, 0x04, 0x00, 0x07, 0x02, 0x05, 0x01, 0x06,

    // White pawns.
    // Since the AI considers all the moves of each piece in turn, I think this
    // ordering of the pawns causes the computer to move the innermost pawns
    // when several pawn moves seem equally good.
    0x10, 0x17, 0x11, 0x16, 0x12, 0x15, 0x14, 0x13,

    // Black pieces and pawns.
    0x73, 0x74, 0x70, 0x77, 0x72, 0x75, 0x71, 0x76,
    0x60, 0x67, 0x61, 0x66, 0x62, 0x65, 0x64, 0x63
};

/*
 *      EXCHANGE SIDES FOR REPLY
 *      ANALYSIS
 */
static void reverse() {
    int x = 15;
    do {
        // SUBTRACT POSITION FROM 77 AND EXCHANGE PIECES
        uint8_t tmp = bk[x];
        bk[x] = 0x77 - board[x];
        board[x] = 0x77 - tmp;
    } while (--x >= 0);
}


/*** Move stack **************************************************************/

struct Move {
    uint8_t m_moven;        // saved state of moven global variable
    int8_t  m_piece;        // index of piece that was moved
    uint8_t m_src;          // where m_piece was before this move
    uint8_t m_captured;     // index of piece that was captured
    uint8_t m_dest;         // where m_piece was moved to
};

#define STACK_SIZE 5
static Move stack[STACK_SIZE];  // [$B9:$C8]
Move *sp2;

static uint8_t moven;
static int8_t piece;
static uint8_t square;

/*
 *      THIS ROUTINE MOVES PIECE
 *      TO SQUARE, PARAMETERS
 *      ARE SAVED IN A STACK TO UNMAKE
 *      THE MOVE LATER
 */
static void move() {
    assert(sp2 > stack);
    sp2--;

    sp2->m_dest = square;                   // TO SQUARE
    int x = 0x1f;

    do {
        if (square == board[x])             // CHECK FOR
            break;                          // CAPTURE
        x--;
    } while (x >= 0);

    board[x] = CAPTURED;
    sp2->m_captured = x;                    // CAPTURED PIECE
    int src = board[piece];
    board[piece] = square;                  // FROM
    sp2->m_src = src;                       // SQUARE
    sp2->m_piece = piece;                   // PIECE
    sp2->m_moven = moven;                   // MOVEN
}

/*
 *      ROUTINE TO UNMAKE A MOVE MADE BY
 *      MOVE
 *
 * The original comments are wrong about which bits of data are being moved,
 * but I kept them. The code is right.
 */
static void umove() {
    moven = sp2->m_moven;                   // MOVEN
    piece = sp2->m_piece;                   // CAPTURED PIECE
    board[piece] = sp2->m_src;              // FROM SQUARE
    square = board[sp2->m_captured] = sp2->m_dest; // PIECE, TO SQUARE
    sp2++;
}


/*** Move generation *********************************************************/

#define ST_CHKCHK  (-7)

static int8_t state;
static bool inchek;

static const uint8_t movex[17] = {
    0x00, 0xf0, 0xff, 0x01, 0x10, 0x11, 0x0f, 0xef, 0xf1,
    0xdf, 0xe1, 0xee, 0xf2, 0x12, 0x0e, 0x1f, 0x21
};

static void janus(bool capture);
static int chkchk(int s);

#define S_ILLEGAL 1
#define S_CAPTURE 2
#define S_ILLCHK 4

/*
 *      CMOVE CALCULATES THE TO SQUARE
 *      USING SQUARE AND THE MOVE
 *      TABLE,  FLAGS SET AS FOLLOWS:
 *      S_ILLEGAL - ILLEGAL MOVE
 *      S_CAPTURE - CAPTURE (LEGAL UNLESS IN CH)
 *      S_ILLCHK - ILLEGAL BECAUSE OF CHECK
 *      [MY THANKS TO JIM BUTTERFIELD
 *      WHO WROTE THIS MORE EFFICIENT
 *      VERSION OF CMOVE]
 *
 * Reads: square, moven, board
 * Writes: square
 */
static int cmove() {
    square = square + movex[moven];         // NEW POS'N
    if (square & 0x88)
        return S_ILLEGAL;                   // OFF BOARD

    int x = 0x20;
    int s;
    for (;;) {
        --x;                                // IS TO SQUARE OCCUPIED?
        if (x < 0) {
            s = 0;
            break;
        }
        if (square == board[x]) {
            if (x < 0x10)                   // BY SELF?
                return S_ILLEGAL;
            s = S_CAPTURE;                  // MUST BE CAP! SET V FLAG
            break;
        }
    }

    if (state < 0 || state >= 8)            // SHOULD WE DO THE CHECK CHECK?
        return s;

    return chkchk(s);
}

/*
 *      REPLACE PIECE ON CORRECT SQUARE
 */
static void reset() {
    square = board[piece];                  // GET LOGAT FOR PIECE FROM BOARD
}

/*
 *      CALCULATE SINGLE STEP MOVES
 *      FOR K,N
 *
 * Returns true if moven is nonzero,
 * i.e. there are more moves to consider.
 */
static bool sngmv() {
    int s = cmove();
    if (!(s & S_ILLEGAL)) {                 // CALC MOVE -IF LEGAL
        assert(s == 0 || s == S_CAPTURE);
        janus(s != 0);                      // -EVALUATE
    }
    reset();
    return --moven != 0;
}

/*
 *      CALCULATE ALL MOVES DOWN A
 *      STRAIGHT LINE FOR Q,B,R
 *
 * Returns true if moven is nonzero,
 * i.e. there are more moves to consider.
 */
static bool line() {
    int s;

    while (true) {
        s = cmove();                        // CALC MOVE
        if (!(s & S_ILLCHK) || (s & S_CAPTURE)) {  // NO CHK, NOCAP
            if (s & S_ILLEGAL)              // RETURN
                break;
            assert(s == 0 || s == S_CAPTURE);
            janus(s != 0);
            if (s & S_CAPTURE)              // NOT A CAP
                break;
        }
    }

    reset();                                // LINE STOPPED
    return --moven != 0;                    // NEXT DIR
}

/*
 * gnm - Generate moves.
 *
 * For each legal move that white can play, set 'piece' to the index of the
 * piece being moved and 'square' to the destination square, and call
 * janus(true) if the move is a capture and janus(false) if it is not.
 *
 * This does not consider en passant or castling.
 */
static void gnm() {
    piece = 16;                             // SET UP PIECE
    while (--piece >= 0) {                  // NEW PIECE. ALL DONE?
        reset();                            // READY
        // GET PIECE
        moven = 8;                          // COMMON START
        if (piece >= 8) {                   // WHAT IS IT? PAWN
            moven = 6;
            do {
                int s = cmove();
                if ((s & (S_CAPTURE | S_ILLEGAL)) == S_CAPTURE) {  // RIGHT CAP?
                    assert(s == 0 || s == S_CAPTURE);
                    janus(s != 0);          // YES
                }
                reset();
            } while (--moven == 5);         // LEFT CAP?
            do {
                int s = cmove();
                if (s & (S_CAPTURE | S_ILLEGAL))  // AHEAD
                    break;                  // ILLEGAL
                assert(s == 0 || s == S_CAPTURE);
                janus(s != 0);
            } while ((square & 0xf0) == 0x20);  // GETS TO 3RD RANK?
        } else if (piece >= 6) {            // KNIGHT
            moven = 16;                     // MOVES
            do {
                sngmv();                    // 16 TO 9
            } while (moven != 8);
        } else if (piece >= 4) {            // BISHOP
            do {
                line();
            } while (moven != 4);           // MOVES 8 TO 5
        } else if (piece == 1) {            // QUEEN
            while (line() != 0) {}          // MOVES 8 TO 1
        } else if (piece > 1) {             // ROOK
            moven = 4;                      // MOVES
            while (line() != 0) {}
        } else {                            // MUST BE KING!
            while (sngmv() != 0) {}         // MOVES 8 TO 1
        }
    }
}

/*
 *      CHKCHK REVERSES SIDES
 *      AND LOOKS FOR A KING
 *      CAPTURE TO INDICATE
 *      ILLEGAL MOVE BECAUSE OF
 *      CHECK  SINCE THIS IS
 *      TIME CONSUMING, IT IS NOT
 *      ALWAYS DONE
 */
static int chkchk(int s) {
    assert(s == 0 || s == S_CAPTURE);

    int8_t saved = state;
    state = ST_CHKCHK;
    inchek = false;
    move();
    reverse();
    gnm();
    reverse();
    umove();
    state = saved;
    if (!inchek)
        return s;                           // NO - SAFE
    return s | S_ILLEGAL | S_ILLCHK;        // YES - IN CHK
}


/*** Main chess strategy *****************************************************/

static uint8_t wcap0;
static uint8_t capstack[5];
#define bcap2 (capstack[0])
#define wcap2 (capstack[1])
#define bcap1 (capstack[2])
#define wcap1 (capstack[3])
#define bcap0 (capstack[4])

struct State {
    uint8_t s_mob;
    uint8_t s_maxc;
    uint8_t s_cc;
    uint8_t s_pcap;
};

static State states[4];
#define bmob  (states[0].s_mob)
#define bmaxc (states[0].s_maxc)
#define bcc   (states[0].s_cc)
#define bmaxp (states[0].s_pcap)
#define xmaxc (states[1].s_maxc)
#define wmob  (states[2].s_mob)
#define wmaxc (states[2].s_maxc)
#define wcc   (states[2].s_cc)
#define wmaxp (states[2].s_pcap)
#define pmob  (states[3].s_mob)
#define pmaxc (states[3].s_maxc)
#define pcc   (states[3].s_cc)
#define pcp   (states[3].s_pcap)

static uint8_t bestp, bestv, bestm;

static const uint8_t points[16] = {
    11, 10, 6, 6, 4, 4, 4, 4,
    2, 2, 2, 2, 2, 2, 2, 2
};

/*
 * GENERATE ALL MOVES FOR ONE
 * SIDE, CALL JANUS AFTER EACH
 * ONE FOR NEXT STEP
 *
 */
static void gnmx(int st) {
    assert(st == 0 || st == 4 || st == 12);
    state = st;
    for (int i = 0; i < 5; i++)             // CLEAR COUNTERS
        capstack[i] = 0;

    // If we are preparing to run in state 12, clear the state-12 counters
    // first.  Otherwise, leave them. They contain data we're going to need.
    int n = st == 12 ? 4 : 3;
    for (int i = 0; i < n; i++) {
        State &st = states[i];
        st.s_mob = 0;
        st.s_maxc = 0;
        st.s_cc = 0;
        st.s_pcap = 0;
    }

    gnm();
}

static void genrm() {
    move();                                 // MAKE MOVE
    reverse();                              // REVERSE BOARD
    gnm();                                  // GENERATE MOVES
    reverse();                              // REVERSE BACK
    umove();
}

/*
 *      IF A PIECE HAS BEEN CAPTURED BY
 *      A TRIAL MOVE, GENERATE REPLIES &
 *      EVALUATE THE EXCHANGE GAIN/LOSS
 */
static void tree(bool capture) {
    assert(state <= 0);
    assert(state > -5);
    assert((square & 0x88) == 0);
    if (!capture)                           // NO CAP
        return;

    int y;
    for (y = 7; square != bk[y];) {         // (PIECES)
        y--;
        if (y == 0)                         // (KING)
            return;
    }                                       // SAVE
    if (points[y] >= capstack[state + 4]) {
        capstack[state + 4] = points[y];
    }
    state--;
    if (state != -5)                        // IF STATE=FB TIME TO TURN AROUND
        genrm();                            // GENERATE FURTHER CAPTURES
    state++;
}

/*
 * After examining all possible outcomes of a move (piece, square)
 * and accumulating counts, compute a score for that move.
 *
 * If the score is better than the best score available so far this turn,
 * update (bestv, bestp, bestm).
 */
static void stratgy() {
    int a = 0x80;

    a += wmob + wmaxc + wcc                 // PARAMETERS WITH WEIGHT OF 0.25
         + wcap1 + wcap2
         - pmaxc - pcc - bcap0 - bcap1 - bcap2 - pmob - bmob;
    if (a < 0)                              // UNDERFLOW
        a = 0;                              // PREVENTION

    a >>= 1;                                // PARAMETERS WITH WEIGHT OF 0.5
    a += 0x40 + wmaxc + wcc - bmaxc;

    a >>= 1;
    a += 0x90
        + 4 * wcap0                         // PARAMETERS WITH WEIGHT OF 1.0
         + wcap1
         - 2 * bmaxc                        // [UNDER OR OVERFLOW MAY OCCUR
         - 2 * bcc                          // FROM THIS SECTION]
         - bcap1;

    if (square == 0x33 ||                   // POSITION BONUS FOR
        square == 0x34 ||                   // MOVE TO CENTRE
        square == 0x22 ||                   // OR OUT OF
        square == 0x25 ||                   // BACK RANK
        !(piece == 0 || board[piece] >= 16))
    {
        a += 2;
    }

    //
    //      CONTINUATION OF SUB STRATGY
    //      -CHECKS FOR CHECK OR CHECKMATE
    //      AND ASSIGNS VALUE TO MOVE
    //
    if (bmaxc == points[0])                 // CAN BLK CAP MY KING?
        a = 0;                              // GULP! DUMB MOVE!
    else if (bmob == 0 && wmaxp == 0)       // IS BLACK UNABLE TO MOVE AND KING IN CH?
        a = 0xff;                           // YES! MATE

    state = 4;                              // RESTORE STATE=4

    //
    //      THE VALUE OF THE MOVE (IN ACCU)
    //      IS COMPARED TO THE BEST MOVE AND
    //      REPLACES IT IF IT IS BETTER
    //
    if (a > bestv) {                        // IS THIS BEST MOVE SO FAR?
        bestv = a;                          // YES!
        bestp = piece;                      // SAVE IT
        bestm = square;                     // FLASH DISPLAY
    }
}

/*
 *      GENERATE FURTHER MOVES FOR COUNT
 *      AND ANALYSIS
 */
static void on4() {
    wcap0 = xmaxc;                          // SAVE ACTUAL CAPTURE
                                            // STATE=0
    move();                                 // GENERATE
    reverse();                              // IMMEDIATE
    gnmx(0);                                // REPLY MOVES
    reverse();

    state = 8;                              // STATE=8; GENERATE
    gnm();                                  // CONTINUATION
    umove();                                // MOVES

    stratgy();                              // FINAL EVALUATION
}

/*
 *      THE ROUTINE JANUS DIRECTS THE
 *      ANALYSIS BY DETERMINING WHAT
 *      SHOULD OCCUR AFTER EACH MOVE
 *      GENERATED BY GNM
 *
 *
 */
static void janus(bool capture) {
    if (state >= 0) {
        if (state == 8 && piece == bmaxp && piece != 0)  // IF STATE=8 DO NOT COUNT BLK MAX CAP MOVES FOR WHITE
            return;

        //
        //      THIS ROUTINE COUNTS OCCURRENCES
        //      IT DEPENDS UPON STATE TO INDEX
        //      THE CORRECT COUNTERS
        //
        assert(state >= 0);
        assert(state <= 12);
        assert((state & 3) == 0);
        State &st = states[state>>2];
        st.s_mob++;                         // MOBILITY
        if (piece == 1)                     //  + QUEEN
            st.s_mob++;                     // FOR TWO
        if (capture) {
            int y = 15;
            while (square != bk[y] && --y >= 0) {}
            assert(y >= 0);
            int pts = points[y];
            if (pts >= st.s_maxc) {         // SAVE IF
                st.s_pcap = y;              // BEST THIS
                st.s_maxc = pts;            // STATE
            }
            st.s_cc += pts;
        }
        if (state == 4)
            on4();
        else if (state < 4)
            tree(capture);
    } else if (state == ST_CHKCHK) {
        //
        //      DETERMINE IF THE KING CAN BE
        //      TAKEN, USED BY CHKCHK
        //
        if (square == bk[0])                // IS KING IN CHECK?
            inchek = true;                  // SET INCHEK=0 IF IT IS
    } else {
        tree(capture);
    }
}


/*** Chess AI entry points and scripted opening ******************************/

#define dis1 bestp
#define dis2 bestv
#define dis3 bestm

static int8_t omove;

/*
 * Microchess knows a 9-move standard canned opening.  This is the opening.
 * It's read by go(), and (like everything else in the program) the array is
 * read backwards.  A global variable 'omove' indexes into this array.
 *
 * The 0xcc at opning[27] refers to the initial state of the "black's last
 * move" variable.
 *
 * Each row, reading from right to left, has:
 * - a number from 0x00 to 0x0f telling which piece white is moving;
 * - the square white's piece moves to;
 * - the square black moves to in response.
 *
 * The computer, playing as white, doesn't care which piece black moves; but if
 * the black move does not end up on the particular space expected, we've gone
 * off script, and the computer switches from this canned opening to the AI.
 *
 * If black follows the script, we get the Giuoco Piano:
 *     https://en.wikipedia.org/wiki/Giuoco_Piano
 * 365chess.com calls 7. Nc3 "Greco's attack".
 *
 * After '9. bxc3', here's what the board might look like:
 *
 *     r . b q k . . r
 *     p p p p . p p p
 *     . . n . . . . .
 *     . . . . . . . .
 *     . . B P n . . .
 *     . . P . . N . .
 *     P . . . . P P P
 *     R . B Q . R K .
 *
 * The last move here, bxc3, is not necessarily better than d5.
 *
 */
static const uint8_t opning[28] = {
    0x99, 0x25, 0x0b,  // 9. bxc3
    0x25, 0x01, 0x00,  // 8. o-o Bxc3 (or Nxc3)
    0x33, 0x25, 0x07,  // 7. Nc3 Nxe4
    0x36, 0x34, 0x0d,  // 6. cxd4 Bb4+ (or Nb4)
    0x34, 0x34, 0x0e,  // 5. d4 exd4 (or Bxd4 or Nxd4)
    0x52, 0x25, 0x0d,  // 4. c3 Nf6 (or f7f6)
    0x45, 0x35, 0x04,  // 3. Bc4 Bc5 (or the unlikely c6c5)
    0x55, 0x22, 0x06,  // 2. Nf3 Nc6 (or the unlikely c7c6)
    0x43, 0x33, 0x0f,  // 1. e4 e5
    0xcc
};

static int mv2() {
    bestv = board[bestp];                   // MOVE THE BEST
    piece = bestp;                          // MOVE
    square = bestm;                         // AND DISPLAY
    move();                                 // IT
    return 0;
}

static int mate() {
    return -1;                              // RESIGN OR STALEMATE
}

/*
 *      MAIN PROGRAM TO PLAY CHESS
 *      PLAY FROM OPENING OR THINK
 */
static int go() {
    if (omove >= 0) {                        // OPENING? -NO   *ADD CHANGE FROM BPL
        if (dis3 == opning[omove]) {        // YES WAS. OPPONENT'S MOVE OK?
            dis1 = opning[--omove];         // GET NEXT CANNED OPENING MOVE
            dis3 = opning[--omove];         // DISPLAY IT
            --omove;                        // MOVE IT
            return mv2();
        }
        omove = -1;                         // *ADD - STOP CANNED MOVES. FLAG OPENING FINISHED
    }
                                            // STATE=C
    bestv = 0x0c;                           // CLEAR BESTV
    gnmx(12);                               // GENERATE P MOVES
    gnmx(4);                                // STATE=4  GENERATE AND TEST AVAILABLE MOVES
    if (bestv < 0x0f)                       // GET BEST MOVE. IF NONE
        return mate();                      // UH OH!
    return mv2();
}


/*** I/O *********************************************************************/

static bool rev = false;

static void disp() {
    int8_t x = 31;
    while (board[x] != dis2 && --x >= 0) {} // DISPLAY PIECE AT FROM
    dis1 = x;                               // SQUARE
    piece = x;
}

static const char *cph = "KQRRBBNNPPPPPPPPkqrrbbnnpppppppp";

/*
 * The following routines were added to allow text-based board
 * display over a standard RS-232 port.
 */
static void pout() {
    char bd[8][8];
    for (int r = 0; r < 8; r++)
        for (int c = 0; c < 8; c++)
            bd[r][c] = '.';

    int flipbit = rev ? 16 : 0;
    for (int p = 0; p < 32; p++) {
        uint8_t square = board[p];
        if ((square & 0x88) == 0)
            bd[(square >> 4) & 7][square & 7] = cph[p ^ flipbit];
    }

    for (int r = 0; r < 8; r++) {
        printf("    ");
        for (int c = 0; c < 8; c++) {
            putchar(bd[r][c]);
            putchar(' ');
        }
        printf("  %d\n", r + 1);
    }
    printf("\n    h g f e d c b a    ");

    printf("    %02X %02X %02X\n",
           (unsigned) (uint8_t) bestp,
           (unsigned) (uint8_t) bestv,
           (unsigned) (uint8_t) bestm);
}

static void setup() {
    memcpy(board, setw, 32);        // SET UP BOARD
    omove = 27;
}


#define LINE_SIZE 256

static void print_square(uint8_t square) {
    putchar('a' + (7 - (square & 7)));
    putchar('1' + ((square >> 4) & 7));
}

static int chess() {
    char line[LINE_SIZE];

    puts("    MicroChess (c) 1996-2005 Peter Jennings, www.benlo.com");
    putchar('\n');
    setup();
    dis1 = dis2 = dis3 = 0xcc;
    pout();

    for (;;) {
        printf("> ");
        fflush(stdout);
        if (!fgets(line, LINE_SIZE, stdin))
            return 1;
        putchar('\n');

        int i = 0;
        while (isspace(line[i]))
            i++;
    
        char c = tolower(line[i]);
        if (c == 'q') {
            return 0;
        } else if (c == 'p') {
            sp2 = stack + STACK_SIZE;
            if (go() != 0) {
                printf("resign\n");
                dis1 = dis2 = dis3 = 0xff;
            } else {
                print_square(bestv);
                print_square(bestm);
                putchar('\n');
            }
            pout();
        } else if (c == 'r') {
            reverse();                      // REVERSE BOARD IS
            rev = !rev;                     // TOGGLE REV FLAG
            dis1 = dis2 = dis3 = 0xee;
            pout();
        } else if ('a' <= c && c <= 'h') {
            int from, to;

            i++;
            from = 7 - (c - 'a');

            c = line[i++];
            if (!('1' <= c && c <= '8')) {
                printf("*** unrecognized command\n");
                continue;
            }
            from |= (c - '1') << 4;

            while (isspace(line[i]) || line[i] == '-')
                i++;

            c = line[i++];
            if (!('a' <= c && c <= 'h')) {
                printf("*** unrecognized command\n");
                continue;
            }
            to = 7 - (c - 'a');

            c = line[i++];
            if (!('1' <= c && c <= '8')) {
                printf("*** unrecognized command\n");
                continue;
            }
            to |= (c - '1') << 4;

            while (isspace(line[i]))
                i++;
            if (line[i] != '\0') {
                printf("*** unrecognized command\n");
                continue;
            }

            bestv = from;
            bestm = to;
            square = dis3;
            disp();
            sp2 = stack + STACK_SIZE;
            move();
            disp();
            pout();
        } else {
            printf("*** unrecognized command\n");
        }
    }
}

int main() {
    return chess();
}
