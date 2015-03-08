//> ;***********************************************************************
//> ;
//> ;  Kim-1 MicroChess (c) 1976-2005 Peter Jennings, www.benlo.com
//> ;
//> ;***********************************************************************
//>
//> ; All rights reserved.
//>
//> ; Redistribution and use in source and binary forms, with or without
//> ; modification, are permitted provided that the following conditions
//> ; are met:
//> ; 1. Redistributions of source code must retain the above copyright
//> ;    notice, this list of conditions and the following disclaimer.
//> ; 2. Redistributions in binary form must reproduce the above copyright
//> ;    notice, this list of conditions and the following disclaimer in the
//> ;    documentation and/or other materials provided with the distribution.
//> ; 3. The name of the author may not be used to endorse or promote products
//> ;    derived from this software without specific prior written permission.
//>
//> ; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR
//> ; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//> ; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
//> ; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
//> ; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
//> ; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//> ; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//> ; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//> ; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
//> ; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//>
//> ;
//> ; modified by Daryl Rictor to work over a
//> ; serial terminal connection, August 2002.
//> ;
//> ; Updated with corrections to earlier OCR errors by Bill Forster, August 2005.
//> ;
//>    cpu 65c02
//>    page 0,132
//> ;
//> ; 6551 I/O Port Addresses
//> ;
//> ACIADat =       $7F70
//> ACIASta =       $7F71
//> ACIACmd =       $7F72
//> ACIACtl =       $7F73

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

// Comment out the following #define if your OS doesn't have <termios.h>.
// This will make the user interface rather bizarre, unfortunately!
#define HAVE_TERMIOS 1

#ifdef HAVE_TERMIOS
#  include <termios.h>
#  include <unistd.h>
#  define GO_KEY '\n'
#else
#  define GO_KEY 'G'
#endif

#define S_ILLEGAL 1
#define S_CAPTURE 2
#define S_ILLCHK 4

static int chess();
static void janus(int s);
static void on4();
static void nocount(int s);
static void tree(int s);
static void input(int a);
static void disp();
static void gnmz();
static void gnmx(int x);
static void gnm();
static bool sngmv();
static bool line();
static void reverse();
static int cmove();
static int spx(int s);
static void reset();
static void genrm();
static void umove();
static void move();
static int ckmate(int a);
static int go();
static int mv2();
static int mate();
static void dismv(int8_t a);
static int stratgy();
static void pout();
static int kin();
static void change_terminal(bool raw);

extern const char *cpl;
extern const char *cph;
extern const uint8_t setw[32];
extern const uint8_t movex[17];
extern const uint8_t points[16];
extern const uint8_t opning[28];

//> ;
//> ; page zero variables
//> ;
//> BOARD   =       $50
//> BK      =       $60
//> PIECE   =       $B0
//> SQUARE  =       $B1
//> SP2     =       $B2
//> SP1     =       $B3
//> INCHEK  =       $B4
//> STATE   =       $B5
//> MOVEN   =       $B6
//> REV     =       $B7
//> OMOVE   =       $DC
//> WCAP0   =       $DD
//> COUNT   =       $DE
//> BCAP2   =       $DE
//> WCAP2   =       $DF
//> BCAP1   =       $E0
//> WCAP1   =       $E1
//> BCAP0   =       $E2
//> MOB     =       $E3
//> MAXC    =       $E4
//> CC      =       $E5
//> PCAP    =       $E6
//> BMOB    =       $E3
//> BMAXC   =       $E4
//> BMCC    =       $E5             ; was BCC (TASS doesn't like it as a label)
//> BMAXP   =       $E6
//> XMAXC   =       $E8
//> WMOB    =       $EB
//> WMAXC   =       $EC
//> WCC     =       $ED
//> WMAXP   =       $EE

//> PMOB    =       $EF
//> PMAXC   =       $F0
//> PCC     =       $F1
//> PCP     =       $F2

//> OLDKY   =       $F3
//> BESTP   =       $FB
//> BESTV   =       $FA
//> BESTM   =       $F9
//> DIS1    =       $FB
//> DIS2    =       $FA
//> DIS3    =       $F9
//> temp    =       $FC
//> ;
//> ;
//> ;

struct Move {
    uint8_t m_moven;        // saved state of moven global variable
    int8_t  m_piece;        // index of piece that was moved
    uint8_t m_src;          // where m_piece was before this move
    uint8_t m_captured;     // index of piece that was captured
    uint8_t m_dest;         // where m_piece was moved to
};

#define CAPTURED ((uint8_t) 0xcc)

static uint8_t board[32];
#define bk (board + 16)
static int8_t piece;
static uint8_t square;
static int8_t inchek;
static int8_t state;
static uint8_t moven, rev;

/*
 * This is something of a mystery. In the original program, the stack could
 * grow to at most 3 moves before overwriting REV and other variables.  But the
 * program pretty clearly recurses... I dunno, setting it to 8 is a stopgap
 * until I understand what's going on.
 */
#define STACK_SIZE 8
static Move stack[STACK_SIZE];  // [$B9:$C8]
Move *sp2;

uint8_t wcap0;
uint8_t capstack[5];
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

State states[4];
#define bmob  (states[0].s_mob)
#define bmaxc (states[0].s_maxc)
#define bmcc  (states[0].s_cc)              // was BCC (TASS doesn't like it as a label)
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

static uint8_t omove;

static uint8_t bestp, bestv, bestm;
#define dis1 bestp
#define dis2 bestv
#define dis3 bestm


//>                 *=$1000                 ; load into RAM @ $1000-$15FF
//>
//>                 LDA     #$00            ; REVERSE TOGGLE
//>                 STA     REV
//>                 JSR     Init_6551
//> CHESS           CLD                     ; INITIALIZE
//>                 LDX     #$FF            ; TWO STACKS
//>                 TXS
//>                 LDX     #$C8
//>                 STX     SP2
//> ;
//> ;       ROUTINES TO LIGHT LED
//> ;       DISPLAY AND GET KEY
//> ;       FROM KEYBOARD
//> ;
//> OUT             JSR     pout            ; DISPLAY AND
//>                 JSR     KIN             ; GET INPUT   *** my routine waits for a keypress
//> ;               CMP     OLDKY           ; KEY IN ACC  *** no need to debounce
//> ;               BEQ     OUT             ; (DEBOUNCE)
//> ;               STA     OLDKY
//> ;
//>                 CMP     #$43            ; [C]
//>                 BNE     NOSET           ; SET UP
//>                 LDX     #$1F            ; BOARD
//> WHSET           LDA     SETW,X          ; FROM
//>                 STA     BOARD,X         ; SETW
//>                 DEX
//>                 BPL     WHSET
//>                 LDX     #$1B            ; *ADDED
//>                 STX     OMOVE           ; INITS TO $FF
//>                 LDA     #$CC            ; Display CCC
//>                 BNE     CLDSP
//> ;
//> NOSET           CMP     #$45            ; [E]
//>                 BNE     NOREV           ; REVERSE
//>                 JSR     REVERSE         ; BOARD IS
//>                 SEC
//>                 LDA     #$01
//>                 SBC     REV
//>                 STA     REV             ; TOGGLE REV FLAG
//>                 LDA     #$EE            ; IS
//>                 BNE     CLDSP
//> ;
//> NOREV           CMP     #$40                    ; [P]
//>                 BNE     NOGO            ; PLAY CHESS
//>                 JSR     GO
//> CLDSP           STA     DIS1            ; DISPLAY
//>                 STA     DIS2            ; ACROSS
//>                 STA     DIS3            ; DISPLAY
//>                 BNE     CHESS
//> ;
//> NOGO            CMP     #$0D            ; [Enter]
//>                 BNE     NOMV            ; MOVE MAN
//>                 JSR     MOVE            ; AS ENTERED
//>                 JMP     DISP
//> NOMV            CMP     #$41            ; [Q] ***Added to allow game exit***
//>                 BEQ     DONE            ; quit the game, exit back to system.
//>                 JMP     INPUT           ; process move
//> DONE            JMP     $FF00           ; *** MUST set this to YOUR OS starting address

int chess() {
    for (;;) {
        sp2 = stack + STACK_SIZE;
        pout();                             // DISPLAY AND
        int c = kin();                      // GET INPUT KEY IN ACC
        if (c == 'C') {
            memcpy(board, setw, 32);        // SET UP BOARD
            omove = 27;
            c = 0xcc;
        } else if (c == 'E') {
            reverse();                      // REVERSE BOARD IS
            rev = 1 - rev;                  // TOGGLE REV FLAG
            c = 0xee;                       // IS
        } else if (c == 0x40) {             // [P]
            // Seems to be a bug. GO sometimes jumps back to CHESS
            // instead of returning. Why doesn't this overflow the stack?
            c = go();
        } else if (c == GO_KEY) {
            move();
            disp();
            continue;
        } else if (c == 0x41) {             // [Q] ***Added to allow game exit***
            putchar('\n');
            change_terminal(false);
            exit(0);
        } else if (c == '\n') {
            // do nothing --jto
            continue;
        } else {
            input(c);
            continue;
        }
        dis1 = c;                           // DISPLAY
        dis2 = c;                           // ACROSS
        dis3 = c;                           // DISPLAY
    }
}

//> ;
//> ;       THE ROUTINE JANUS DIRECTS THE
//> ;       ANALYSIS BY DETERMINING WHAT
//> ;       SHOULD OCCUR AFTER EACH MOVE
//> ;       GENERATED BY GNM
//> ;
//> ;
//> ;
//> JANUS           LDX     STATE
//>                 BMI     NOCOUNT
//> ;
//> ;       THIS ROUTINE COUNTS OCCURRENCES
//> ;       IT DEPENDS UPON STATE TO INDEX
//> ;       THE CORRECT COUNTERS
//> ;
//> COUNTS          LDA     PIECE
//>                 BEQ     OVER            ; IF STATE=8
//>                 CPX     #$08            ; DO NOT COUNT
//>                 BNE     OVER            ; BLK MAX CAP
//>                 CMP     BMAXP           ; MOVES FOR
//>                 BEQ     XRT             ; WHITE
//> ;
//> OVER            INC     MOB,X           ; MOBILITY
//>                 CMP     #$01            ;  + QUEEN
//>                 BNE     NOQ             ; FOR TWO
//>                 INC     MOB,X
//> ;
//> NOQ             BVC     NOCAP
//>                 LDY     #$0F            ; CALCULATE
//>                 LDA     SQUARE          ; POINTS
//> ELOOP           CMP     BK,Y            ; CAPTURED
//>                 BEQ     FOUN            ; BY THIS
//>                 DEY                     ; MOVE
//>                 BPL     ELOOP
//> FOUN            LDA     POINTS,Y
//>                 CMP     MAXC,X
//>                 BCC     LESS            ; SAVE IF
//>                 STY     PCAP,X          ; BEST THIS
//>                 STA     MAXC,X          ; STATE
//> ;
//> LESS            CLC
//>                 PHP                     ; ADD TO
//>                 ADC     CC,X            ; CAPTURE
//>                 STA     CC,X            ; COUNTS
//>                 PLP
//> ;
//> NOCAP           CPX     #$04
//>                 BEQ     ON4
//>                 BMI     TREE            ;(=00 ONLY)
//> XRT             RTS

/*
 *      THE ROUTINE JANUS DIRECTS THE
 *      ANALYSIS BY DETERMINING WHAT
 *      SHOULD OCCUR AFTER EACH MOVE
 *      GENERATED BY GNM
 *
 *
 */
void janus(int s) {
    assert(s == 0 || s == S_CAPTURE);
    if (state < 0) {
        nocount(s);
        return;
    }

    //
    //      THIS ROUTINE COUNTS OCCURRENCES
    //      IT DEPENDS UPON STATE TO INDEX
    //      THE CORRECT COUNTERS
    //
    if (piece == 0 || state != 8 || piece != bmaxp) {  // IF STATE=8 DO NOT COUNT BLK MAX CAP MOVES FOR WHITE
        assert(state >= 0);
        assert(state <= 12);
        assert((state & 3) == 0);
        State &st = states[state>>2];
        st.s_mob++;                         // MOBILITY
        if (piece == 1)                     //  + QUEEN
            st.s_mob++;                     // FOR TWO
        if (s & S_CAPTURE) {
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
            tree(s);
    }
}

//> ;
//> ;      GENERATE FURTHER MOVES FOR COUNT
//> ;      AND ANALYSIS
//> ;
//> ON4             LDA     XMAXC           ; SAVE ACTUAL
//>                 STA     WCAP0           ; CAPTURE
//>                 LDA     #$00            ; STATE=0
//>                 STA     STATE
//>                 JSR     MOVE            ; GENERATE
//>                 JSR     REVERSE         ; IMMEDIATE
//>                 JSR     GNMZ            ; REPLY MOVES
//>                 JSR     REVERSE
//> ;
//>                 LDA     #$08            ; STATE=8
//>                 STA     STATE           ; GENERATE
//>                 JSR     GNM             ; CONTINUATION
//>                 JSR     UMOVE           ; MOVES
//> ;
//>                 JMP     STRATGY         ; FINAL EVALUATION

//
//      GENERATE FURTHER MOVES FOR COUNT
//      AND ANALYSIS
//
void on4() {
    wcap0 = xmaxc;                          // SAVE ACTUAL CAPTURE
    state = 0;                              // STATE=0
    move();                                 // GENERATE
    reverse();                              // IMMEDIATE
    gnmz();                                 // REPLY MOVES
    reverse();

    state = 8;                              // STATE=8; GENERATE
    gnm();                                  // CONTINUATION
    umove();                                // MOVES

    stratgy();                              // FINAL EVALUATION
}

//> NOCOUNT CPX     #$F9
//>                 BNE     TREE
//> ;
//> ;      DETERMINE IF THE KING CAN BE
//> ;      TAKEN, USED BY CHKCHK
//> ;
//>                 LDA     BK              ; IS KING
//>                 CMP     SQUARE          ; IN CHECK?
//>                 BNE     RETJ            ; SET INCHEK=0
//>                 LDA     #$00            ; IF IT IS
//>                 STA     INCHEK
//> RETJ            RTS

void nocount(int s) {
    assert(s == 0 || s == S_CAPTURE);
    if (state != -7) {
        tree(s);
    } else {
        //
        // DETERMINE IF THE KING CAN BE
        // TAKEN, USED BY CHKCHK
        //
        if (bk[0] == square)                // IS KING IN CHECK?
            inchek = 0;                     // SET INCHEK=0 IF IT IS
    }
}

//> ;
//> ;      IF A PIECE HAS BEEN CAPTURED BY
//> ;      A TRIAL MOVE, GENERATE REPLIES &
//> ;      EVALUATE THE EXCHANGE GAIN/LOSS
//> ;
//> TREE            BVC     RETJ            ; NO CAP
//>                 LDY     #$07            ; (PIECES)
//>                 LDA     SQUARE
//> LOOPX           CMP     BK,Y
//>                 BEQ     FOUNX
//>                 DEY
//>                 BEQ     RETJ            ; (KING)
//>                 BPL     LOOPX           ; SAVE
//> FOUNX           LDA     POINTS,Y        ; BEST CAP
//>                 CMP     BCAP0,X         ; AT THIS
//>                 BCC     NOMAX           ; LEVEL
//>                 STA     BCAP0,X
//> NOMAX           DEC     STATE
//>                 LDA     #$FB            ; IF STATE=FB
//>                 CMP     STATE           ; TIME TO TURN
//>                 BEQ     UPTREE          ; AROUND
//>                 JSR     GENRM           ; GENERATE FURTHER
//> UPTREE          INC     STATE           ; CAPTURES
//>                 RTS

//
//      IF A PIECE HAS BEEN CAPTURED BY
//      A TRIAL MOVE, GENERATE REPLIES &
//      EVALUATE THE EXCHANGE GAIN/LOSS
//
void tree(int s) {
    assert(state <= 0);
    assert(state > -5);
    assert(s == 0 || s == S_CAPTURE);
    assert((square & 0x88) == 0);
    if (!s)                                 // NO CAP
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

//> ;
//> ;      THE PLAYER'S MOVE IS INPUT
//> ;
//> INPUT           CMP     #$08            ; NOT A LEGAL
//>                 BCS     ERROR           ; SQUARE #
//>                 JSR     DISMV
//> DISP            LDX     #$1F
//> SEARCH          LDA     BOARD,X
//>                 CMP     DIS2
//>                 BEQ     HERE            ; DISPLAY
//>                 DEX                     ; PIECE AT
//>                 BPL     SEARCH          ; FROM
//> HERE            STX     DIS1            ; SQUARE
//>                 STX     PIECE
//> ERROR           JMP     CHESS

void input(int c) {
    if (c < 8) {                           // A LEGAL SQUARE #
        dismv(c);
        disp();
    }
}

void disp() {
    int8_t x = 31;
    while (board[x] != dis2 && --x >= 0) {} // DISPLAY PIECE AT FROM
    dis1 = x;                               // SQUARE
    piece = x;
}

//> ;
//> ;      GENERATE ALL MOVES FOR ONE
//> ;      SIDE, CALL JANUS AFTER EACH
//> ;      ONE FOR NEXT STEP
//> ;
//> ;
//> GNMZ            LDX     #$10            ; CLEAR
//> GNMX            LDA     #$00            ; COUNTERS
//> CLEAR           STA     COUNT,X
//>                 DEX
//>                 BPL     CLEAR
//> ;
//> GNM             LDA     #$10            ; SET UP
//>                 STA     PIECE           ; PIECE
//> NEWP            DEC     PIECE           ; NEW PIECE
//>                 BPL     NEX             ; ALL DONE?
//>                 RTS                     ;    -YES
//> ;
//> NEX             JSR     RESET           ; READY
//>                 LDY     PIECE           ; GET PIECE
//>                 LDX     #$08
//>                 STX     MOVEN           ; COMMON START
//>                 CPY     #$08            ; WHAT IS IT?
//>                 BPL     PAWN            ; PAWN
//>                 CPY     #$06
//>                 BPL     KNIGHT          ; KNIGHT
//>                 CPY     #$04
//>                 BPL     BISHOP          ; BISHOP
//>                 CPY     #$01
//>                 BEQ     QUEEN           ; QUEEN
//>                 BPL     ROOK            ; ROOK
//> ;
//> KING            JSR     SNGMV           ; MUST BE KING!
//>                 BNE     KING            ; MOVES
//>                 BEQ     NEWP            ; 8 TO 1
//> QUEEN           JSR     LINE
//>                 BNE     QUEEN           ; MOVES
//>                 BEQ     NEWP            ; 8 TO 1
//> ;
//> ROOK            LDX     #$04
//>                 STX     MOVEN           ; MOVES
//> AGNR            JSR     LINE            ; 4 TO 1
//>                 BNE     AGNR
//>                 BEQ     NEWP
//> ;
//> BISHOP          JSR     LINE
//>                 LDA     MOVEN           ; MOVES
//>                 CMP     #$04            ; 8 TO 5
//>                 BNE     BISHOP
//>                 BEQ     NEWP
//> ;
//> KNIGHT          LDX     #$10
//>                 STX     MOVEN           ; MOVES
//> AGNN            JSR     SNGMV           ; 16 TO 9
//>                 LDA     MOVEN
//>                 CMP     #$08
//>                 BNE     AGNN
//>                 BEQ     NEWP
//> ;
//> PAWN            LDX     #$06
//>                 STX     MOVEN
//> P1              JSR     CMOVE           ; RIGHT CAP?
//>                 BVC     P2
//>                 BMI     P2
//>                 JSR     JANUS           ; YES
//> P2              JSR     RESET
//>                 DEC     MOVEN           ; LEFT CAP?
//>                 LDA     MOVEN
//>                 CMP     #$05
//>                 BEQ     P1
//> P3              JSR     CMOVE           ; AHEAD
//>                 BVS     NEWP            ; ILLEGAL
//>                 BMI     NEWP
//>                 JSR     JANUS
//>                 LDA     SQUARE          ; GETS TO
//>                 AND     #$F0            ; 3RD RANK?
//>                 CMP     #$20
//>                 BEQ     P3              ; DO DOUBLE
//>                 JMP     NEWP

/*
 * GENERATE ALL MOVES FOR ONE
 * SIDE, CALL JANUS AFTER EACH
 * ONE FOR NEXT STEP
 *
 */
void gnmz() { gnmx(16); }

void gnmx(int x) {
    assert(x == 16 || x == 20);
    for (int i = 0; i < 5; i++)             // CLEAR COUNTERS
        capstack[i] = 0;
    for (int i = 0; i < ((x - 4) >> 2); i++) {
        State &st = states[i];
        st.s_mob = 0;
        st.s_maxc = 0;
        st.s_cc = 0;
        st.s_pcap = 0;
    }
    gnm();
}

/*
 * gnm - Generate moves.
 *
 * For each legal move that white can play, set 'piece' to the index of the
 * piece being moved and 'square' to the destination square, and call
 * janus(S_CAPTURE) if the move is a capture and janus(0) if it is not.
 *
 * This does not consider en passant or castling.
 *
 * Uses piece, moven, square,
 */
void gnm() {
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
                    janus(s);               // YES
                }
                reset();
            } while (--moven == 5);         // LEFT CAP?
            do {
                int s = cmove();
                if (s & (S_CAPTURE | S_ILLEGAL))  // AHEAD
                    break;                  // ILLEGAL
                janus(s);
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


//> ;
//> ;      CALCULATE SINGLE STEP MOVES
//> ;      FOR K,N
//> ;
//> SNGMV           JSR     CMOVE           ; CALC MOVE
//>                 BMI     ILL1            ; -IF LEGAL
//>                 JSR     JANUS           ; -EVALUATE
//> ILL1            JSR     RESET
//>                 DEC     MOVEN
//>                 RTS

// CALCULATE SINGLE STEP MOVES
// FOR K,N
// Returns true if moven is nonzero,
// i.e. there are more moves to consider.
bool sngmv() {
    int s = cmove();
    if (!(s & S_ILLEGAL))                   // CALC MOVE -IF LEGAL
        janus(s);                           // -EVALUATE
    reset();
    return --moven != 0;
}

//> ;
//> ;     CALCULATE ALL MOVES DOWN A
//> ;     STRAIGHT LINE FOR Q,B,R
//> ;
//> LINE            JSR     CMOVE           ; CALC MOVE
//>                 BCC     OVL             ; NO CHK
//>                 BVC     LINE            ; NOCAP
//> OVL             BMI     ILL             ; RETURN
//>                 PHP
//>                 JSR     JANUS           ; EVALUATE POSN
//>                 PLP
//>                 BVC     LINE            ; NOT A CAP
//> ILL             JSR     RESET           ; LINE STOPPED
//>                 DEC     MOVEN           ; NEXT DIR
//>                 RTS

/*
 *      CALCULATE ALL MOVES DOWN A
 *      STRAIGHT LINE FOR Q,B,R
 *
 * Returns true if moven is nonzero,
 * i.e. there are more moves to consider.
 */
bool line() {
    int s;

    while (true) {
        s = cmove();                        // CALC MOVE
        if (!(s & S_ILLCHK) || (s & S_CAPTURE)) {  // NO CHK, NOCAP
            if (s & S_ILLEGAL)              // RETURN
                break;
            janus(s);
            if (s & S_CAPTURE)              // NOT A CAP
                break;
        }
    }

    reset();                                // LINE STOPPED
    return --moven != 0;                    // NEXT DIR
}

//> ;
//> ;      EXCHANGE SIDES FOR REPLY
//> ;      ANALYSIS
//> ;
//> REVERSE         LDX     #$0F
//> ETC             SEC
//>                 LDY     BK,X            ; SUBTRACT
//>                 LDA     #$77            ; POSITION
//>                 SBC     BOARD,X         ; FROM 77
//>                 STA     BK,X
//>                 STY     BOARD,X         ; AND
//>                 SEC
//>                 LDA     #$77            ; EXCHANGE
//>                 SBC     BOARD,X         ; PIECES
//>                 STA     BOARD,X
//>                 DEX
//>                 BPL     ETC
//>                 RTS

/*
 *      EXCHANGE SIDES FOR REPLY
 *      ANALYSIS
 */
void reverse() {
    int x = 15;
    do {
        // SUBTRACT POSITION FROM 77 AND EXCHANGE PIECES
        uint8_t tmp = bk[x];
        bk[x] = 0x77 - board[x];
        board[x] = 0x77 - tmp;
    } while (--x >= 0);
}

//> ;
//> ;        CMOVE CALCULATES THE TO SQUARE
//> ;        USING SQUARE AND THE MOVE
//> ;       TABLE,  FLAGS SET AS FOLLOWS:
//> ;       N - ILLEGAL MOVE
//> ;       V - CAPTURE (LEGAL UNLESS IN CH)
//> ;       C - ILLEGAL BECAUSE OF CHECK
//> ;       [MY THANKS TO JIM BUTTERFIELD
//> ;        WHO WROTE THIS MORE EFFICIENT
//> ;        VERSION OF CMOVE]
//> ;
//> CMOVE           LDA     SQUARE          ; GET SQUARE
//>                 LDX     MOVEN           ; MOVE POINTER
//>                 CLC
//>                 ADC     MOVEX,X         ; MOVE LIST
//>                 STA     SQUARE          ; NEW POS'N
//>                 AND     #$88
//>                 BNE     ILLEGAL         ; OFF BOARD
//>                 LDA     SQUARE
//> ;
//>                 LDX     #$20
//> LOOP            DEX                     ; IS TO
//>                 BMI     NO              ; SQUARE
//>                 CMP     BOARD,X         ; OCCUPIED?
//>                 BNE     LOOP
//> ;
//>                 CPX     #$10            ; BY SELF?
//>                 BMI     ILLEGAL
//> ;
//>                 LDA     #$7F            ; MUST BE CAP!
//>                 ADC     #$01            ; SET V FLAG
//>                 BVS     SPX             ; (JMP)
//> ;
//> NO              CLV                     ; NO CAPTURE
//> ;
//> SPX             LDA     STATE           ; SHOULD WE
//>                 BMI     RETL            ; DO THE
//>                 CMP     #$08            ; CHECK CHECK?
//>                 BPL     RETL

//> ;
//> ;        CHKCHK REVERSES SIDES
//> ;       AND LOOKS FOR A KING
//> ;       CAPTURE TO INDICATE
//> ;       ILLEGAL MOVE BECAUSE OF
//> ;       CHECK  SINCE THIS IS
//> ;       TIME CONSUMING, IT IS NOT
//> ;       ALWAYS DONE
//> ;
//> CHKCHK          PHA                     ; STATE
//>                 PHP
//>                 LDA     #$F9
//>                 STA     STATE           ; GENERATE
//>                 STA     INCHEK          ; ALL REPLY
//>                 JSR     MOVE            ; MOVES TO
//>                 JSR     REVERSE         ; SEE IF KING
//>                 JSR     GNM             ; IS IN
//>                 JSR     RUM             ; CHECK
//>                 PLP
//>                 PLA
//>                 STA     STATE
//>                 LDA     INCHEK
//>                 BMI     RETL            ; NO - SAFE
//>                 SEC                     ; YES - IN CHK
//>                 LDA     #$FF
//>                 RTS
//> ;
//> RETL            CLC                     ; LEGAL
//>                 LDA     #$00            ; RETURN
//>                 RTS
//> ;
//> ILLEGAL LDA     #$FF
//>                 CLC                     ; ILLEGAL
//>                 CLV                     ; RETURN
//>                 RTS

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
 */
int cmove() {
    square = square + movex[moven];         // NEW POS'N
    if (square & 0x88)
        return S_ILLEGAL;                   // OFF BOARD

    int x = 0x20;
    do {
        --x;                                // IS TO SQUARE OCCUPIED?
        if (x < 0)
            return spx(0);
    } while (square != board[x]);
    if (x < 0x10)                           // BY SELF?
        return S_ILLEGAL;
    return spx(S_CAPTURE);                  // MUST BE CAP! SET V FLAG
}

int spx(int s) {
    assert(s == 0 || s == S_CAPTURE);

    if (state < 0 || state >= 8)            // SHOULD WE DO THE CHECK CHECK?
        return s;

    //
    //      CHKCHK REVERSES SIDES
    //      AND LOOKS FOR A KING
    //      CAPTURE TO INDICATE
    //      ILLEGAL MOVE BECAUSE OF
    //      CHECK  SINCE THIS IS
    //      TIME CONSUMING, IT IS NOT
    //      ALWAYS DONE
    //
    int8_t saved = state;
    state = -7;
    inchek = -7;
    move();
    reverse();
    gnm();
    reverse();
    umove();
    state = saved;
    if (inchek < 0)
        return s;                           // NO - SAFE
    return S_ILLEGAL | S_ILLCHK;            // YES - IN CHK
}

//> ;
//> ;       REPLACE PIECE ON CORRECT SQUARE
//> ;
//> RESET           LDX     PIECE           ; GET LOGAT
//>                 LDA     BOARD,X         ; FOR PIECE
//>                 STA     SQUARE          ; FROM BOARD
//>                 RTS

/*
 *      REPLACE PIECE ON CORRECT SQUARE
 */
void reset() {
    square = board[piece];                  // GET LOGAT FOR PIECE FROM BOARD
}

//> ;
//> ;
//> ;
//> GENRM           JSR     MOVE            ; MAKE MOVE
//> GENR2           JSR     REVERSE         ; REVERSE BOARD
//>                 JSR     GNM             ; GENERATE MOVES
//> RUM             JSR     REVERSE         ; REVERSE BACK

void genrm() {
    move();                                 // MAKE MOVE
    reverse();                              // REVERSE BOARD
    gnm();                                  // GENERATE MOVES
    reverse();                              // REVERSE BACK
    umove();
}

//> ;
//> ;       ROUTINE TO UNMAKE A MOVE MADE BY
//> ;         MOVE
//> ;
//> UMOVE           TSX                     ; UNMAKE MOVE
//>                 STX     SP1
//>                 LDX     SP2             ; EXCHANGE
//>                 TXS                     ; STACKS
//>                 PLA                     ; MOVEN
//>                 STA     MOVEN
//>                 PLA                     ; CAPTURED
//>                 STA     PIECE           ; PIECE
//>                 TAX
//>                 PLA                     ; FROM SQUARE
//>                 STA     BOARD,X
//>                 PLA                     ; PIECE
//>                 TAX
//>                 PLA                     ; TO SOUARE
//>                 STA     SQUARE
//>                 STA     BOARD,X
//>                 JMP     STRV

/*
 *      ROUTINE TO UNMAKE A MOVE MADE BY
 *      MOVE
 *
 * The original comments are wrong about which bits of data are being moved,
 * but I kept them. The code is right.
 */
void umove() {
    moven = sp2->m_moven;                   // MOVEN
    piece = sp2->m_piece;                   // CAPTURED PIECE
    board[piece] = sp2->m_src;              // FROM SQUARE
    square = board[sp2->m_captured] = sp2->m_dest; // PIECE, TO SQUARE
    sp2++;
}

//> ;
//> ;       THIS ROUTINE MOVES PIECE
//> ;       TO SQUARE, PARAMETERS
//> ;       ARE SAVED IN A STACK TO UNMAKE
//> ;       THE MOVE LATER
//> ;
//> MOVE            TSX
//>                 STX     SP1             ; SWITCH
//>                 LDX     SP2             ; STACKS
//>                 TXS
//>                 LDA     SQUARE
//>                 PHA                     ; TO SQUARE
//>                 TAY
//>                 LDX     #$1F
//> CHECK           CMP     BOARD,X         ; CHECK FOR
//>                 BEQ     TAKE            ; CAPTURE
//>                 DEX
//>                 BPL     CHECK
//> TAKE            LDA     #$CC
//>                 STA     BOARD,X
//>                 TXA                     ; CAPTURED
//>                 PHA                     ; PIECE
//>                 LDX     PIECE
//>                 LDA     BOARD,X
//>                 STY     BOARD,X         ; FROM
//>                 PHA                     ; SQUARE
//>                 TXA
//>                 PHA                     ; PIECE
//>                 LDA     MOVEN
//>                 PHA                     ; MOVEN
//> STRV            TSX
//>                 STX     SP2             ; SWITCH
//>                 LDX     SP1             ; STACKS
//>                 TXS                     ; BACK
//>                 RTS

/*
 *      THIS ROUTINE MOVES PIECE
 *      TO SQUARE, PARAMETERS
 *      ARE SAVED IN A STACK TO UNMAKE
 *      THE MOVE LATER
 */
void move() {
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

//> ;
//> ;       CONTINUATION OF SUB STRATGY
//> ;       -CHECKS FOR CHECK OR CHECKMATE
//> ;       AND ASSIGNS VALUE TO MOVE
//> ;
//> CKMATE          LDX     BMAXC           ; CAN BLK CAP
//>                 CPX     POINTS          ; MY KING?
//>                 BNE     NOCHEK
//>                 LDA     #$00            ; GULP!
//>                 BEQ     RETV            ; DUMB MOVE!
//> ;
//> NOCHEK  LDX     BMOB                    ; IS BLACK
//>                 BNE     RETV            ; UNABLE TO
//>                 LDX     WMAXP           ; MOVE AND
//>                 BNE     RETV            ; KING IN CH?
//>                 LDA     #$FF            ; YES! MATE
//> ;
//> RETV            LDX     #$04            ; RESTORE
//>                 STX     STATE           ; STATE=4
//> ;
//> ;       THE VALUE OF THE MOVE (IN ACCU)
//> ;       IS COMPARED TO THE BEST MOVE AND
//> ;       REPLACES IT IF IT IS BETTER
//> ;
//> PUSH            CMP     BESTV           ; IS THIS BEST
//>                 BCC     RETP            ; MOVE SO FAR?
//>                 BEQ     RETP
//>                 STA     BESTV           ; YES!
//>                 LDA     PIECE           ; SAVE IT
//>                 STA     BESTP
//>                 LDA     SQUARE
//>                 STA     BESTM           ; FLASH DISPLAY
//> RETP            LDA     #"."            ; print ... instead of flashing disp
//>                 Jmp     syschout        ; print . and return

/*
 *      CONTINUATION OF SUB STRATGY
 *      -CHECKS FOR CHECK OR CHECKMATE
 *      AND ASSIGNS VALUE TO MOVE
 */
int ckmate(int a) {
    if (bmaxc == points[0])                 // CAN BLK CAP MY KING?
        a = 0;                              // GULP! DUMB MOVE!
    else if (bmob == 0 && wmaxp == 0)       // IS BLACK UNABLE TO MOVE AND KING IN CH?
        a = 0xff;                           // YES! MATE

    state = 4;                              // RESTORE STATE=4

    //
    // THE VALUE OF THE MOVE (IN ACCU)
    // IS COMPARED TO THE BEST MOVE AND
    // REPLACES IT IF IT IS BETTER
    //
    if (a > bestv) {                        // IS THIS BEST MOVE SO FAR?
        bestv = a;                          // YES!
        bestp = piece;                      // SAVE IT
        bestm = square;                     // FLASH DISPLAY
    }
    putchar('.');                           // print ... instead of flashing disp
    fflush(stdout);                         // print . and return
    return a;
}

//> ;
//> ;       MAIN PROGRAM TO PLAY CHESS
//> ;       PLAY FROM OPENING OR THINK
//> ;
//> GO              LDX     OMOVE           ; OPENING?
//>                 BMI     NOOPEN          ; -NO   *ADD CHANGE FROM BPL
//>                 LDA     DIS3            ; -YES WAS
//>                 CMP     OPNING,X        ; OPPONENT'S
//>                 BNE     END             ; MOVE OK?
//>                 DEX
//>                 LDA     OPNING,X        ; GET NEXT
//>                 STA     DIS1            ; CANNED
//>                 DEX                     ; OPENING MOVE
//>                 LDA     OPNING,X
//>                 STA     DIS3            ; DISPLAY IT
//>                 DEX
//>                 STX     OMOVE           ; MOVE IT
//>                 BNE     MV2             ; (JMP)
//> ;
//> END             LDA     #$FF            ; *ADD - STOP CANNED MOVES
//>                 STA     OMOVE           ; FLAG OPENING
//> NOOPEN  LDX     #$0C                    ; FINISHED
//>                 STX     STATE           ; STATE=C
//>                 STX     BESTV           ; CLEAR BESTV
//>                 LDX     #$14            ; GENERATE P
//>                 JSR     GNMX            ; MOVES
//> ;
//>                 LDX     #$04            ; STATE=4
//>                 STX     STATE           ; GENERATE AND
//>                 JSR     GNMZ            ; TEST AVAILABLE
//> ;       MOVES
//> ;
//>                 LDX     BESTV           ; GET BEST MOVE
//>                 CPX     #$0F            ; IF NONE
//>                 BCC     MATE            ; OH OH!
//> ;
//> MV2             LDX     BESTP           ; MOVE
//>                 LDA     BOARD,X         ; THE
//>                 STA     BESTV           ; BEST
//>                 STX     PIECE           ; MOVE
//>                 LDA     BESTM
//>                 STA     SQUARE          ; AND DISPLAY
//>                 JSR     MOVE            ; IT
//>                 JMP     CHESS
//> ;

/*
 *      MAIN PROGRAM TO PLAY CHESS
 *      PLAY FROM OPENING OR THINK
 */
int go() {
    if (omove >= 0) {                        // OPENING? -NO   *ADD CHANGE FROM BPL
        if (dis3 == opning[omove]) {        // YES WAS. OPPONENT'S MOVE OK?
            dis1 = opning[--omove];         // GET NEXT CANNED OPENING MOVE
            dis3 = opning[--omove];         // DISPLAY IT
            --omove;                        // MOVE IT
            return mv2();
        }
        omove = -1;                         // *ADD - STOP CANNED MOVES. FLAG OPENING FINISHED
    }
    state = 0x0c;                           // STATE=C
    bestv = 0x0c;                           // CLEAR BESTV
    gnmx(20);                               // GENERATE P MOVES
    state = 4;                              // STATE=4  GENERATE AND
    gnmz();                                 // TEST AVAILABLE MOVES
    if (bestv < 0x0f)                       // GET BEST MOVE. IF NONE
        return mate();                      // UH OH!
    return mv2();
}

int mv2() {
    bestv = board[bestp];                   // MOVE THE BEST
    piece = bestp;                          // MOVE
    square = bestm;                         // AND DISPLAY
    move();                                 // IT
    return chess();
}

//> MATE            LDA     #$FF            ; RESIGN
//>                 RTS                     ; OR STALEMATE

int mate() {
    change_terminal(false);
    exit(1);                                // RESIGN OR STALEMATE
}

//> ;
//> ;       SUBROUTINE TO ENTER THE
//> ;       PLAYER'S MOVE
//> ;
//> DISMV           LDX     #$04            ; ROTATE
//> DROL            ASL     DIS3            ; KEY
//>                 ROL     DIS2            ; INTO
//>                 DEX                     ; DISPLAY
//>                 BNE     DROL            ;
//>                 ORA     DIS3
//>                 STA     DIS3
//>                 STA     SQUARE
//>                 RTS

/*
 *      SUBROUTINE TO ENTER THE
 *      PLAYER'S MOVE
 */
void dismv(int8_t a) {
    dis2 = (dis2 << 4) | (dis3 >> 4);       // ROTATE KEY
    dis3 = (dis3 << 4) | a;                 // INTO DISPLAY
    square = dis3;
}

//> ;
//> ;       THE FOLLOWING SUBROUTINE ASSIGNS
//> ;       A VALUE TO THE MOVE UNDER
//> ;       CONSIDERATION AND RETURNS IT IN
//> ;       THE ACCUMULATOR
//> ;
//>
//> STRATGY CLC
//>                 LDA     #$80
//>                 ADC     WMOB            ; PARAMETERS
//>                 ADC     WMAXC           ; WITH WEIGHT
//>                 ADC     WCC             ; OF 0.25
//>                 ADC     WCAP1
//>                 ADC     WCAP2
//>                 SEC
//>                 SBC     PMAXC
//>                 SBC     PCC
//>                 SBC     BCAP0
//>                 SBC     BCAP1
//>                 SBC     BCAP2
//>                 SBC     PMOB
//>                 SBC     BMOB
//>                 BCS     POS             ; UNDERFLOW
//>                 LDA     #$00            ; PREVENTION
//> POS             LSR
//>                 CLC                     ; **************
//>                 ADC     #$40
//>                 ADC     WMAXC           ; PARAMETERS
//>                 ADC     WCC             ; WITH WEIGHT
//>                 SEC                     ; OF 0.5
//>                 SBC     BMAXC
//>                 LSR                     ; **************
//>                 CLC
//>                 ADC     #$90
//>                 ADC     WCAP0           ; PARAMETERS
//>                 ADC     WCAP0           ; WITH WEIGHT
//>                 ADC     WCAP0           ; OF 1.0
//>                 ADC     WCAP0
//>                 ADC     WCAP1
//>                 SEC                     ; [UNDER OR OVER-
//>                 SBC     BMAXC           ; FLOW MAY OCCUR
//>                 SBC     BMAXC           ; FROM THIS
//>                 SBC     BMCC            ; SECTION]
//>                 SBC     BMCC
//>                 SBC     BCAP1
//>                 LDX     SQUARE          ; ***************
//>                 CPX     #$33
//>                 BEQ     POSN            ; POSITION
//>                 CPX     #$34            ; BONUS FOR
//>                 BEQ     POSN            ; MOVE TO
//>                 CPX     #$22            ; CENTRE
//>                 BEQ     POSN            ; OR
//>                 CPX     #$25            ; OUT OF
//>                 BEQ     POSN            ; BACK RANK
//>                 LDX     PIECE
//>                 BEQ     NOPOSN
//>                 LDY     BOARD,X
//>                 CPY     #$10
//>                 BPL     NOPOSN
//> POSN            CLC
//>                 ADC     #$02
//> NOPOSN  JMP     CKMATE                  ; CONTINUE
//>
//>

int stratgy() {
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
         - 2 * bmcc                         // FROM THIS SECTION]
         - bcap1;

    if (square == 0x33 ||                   // POSITION BONUS FOR
        square == 0x34 ||                   // MOVE TO CENTRE
        square == 0x22 ||                   // OR OUT OF
        square == 0x25 ||                   // BACK RANK
        !(piece == 0 || board[piece] >= 16))
    {
        a += 2;
    }
    return ckmate(a);                        // CONTINUE
}

//> ;-----------------------------------------------------------------
//> ; The following routines were added to allow text-based board
//> ; display over a standard RS-232 port.
//> ;
//> POUT            jsr     pout9           ; print CRLF
//>                 jsr     pout13          ; print copyright
//>                 JSR     POUT10          ; print column labels
//>                 LDY     #$00            ; init board location
//>                 JSR     POUT5           ; print board horz edge
//> POUT1           lDA     #"|"            ; print vert edge
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - SPACE
//>                 LDX     #$1F
//> POUT2           TYA                     ; scan the pieces for a location match
//>                 CMP     BOARD,X         ; match found?
//>                 BEQ     POUT4           ; yes; print the piece's color and type
//>                 DEX                     ; no
//>                 BPL     POUT2           ; if not the last piece, try again
//>                 tya                     ; empty square
//>                 and     #$01            ; odd or even column?
//>                 sta     temp            ; save it
//>                 tya                     ; is the row odd or even
//>                 lsr                     ; shift column right 4 spaces
//>                 lsr                     ;
//>                 lsr                     ;
//>                 lsr                     ;
//>                 and     #$01            ; strip LSB
//>                 clc                     ;
//>                 adc     temp            ; combine row & col to determine square color
//>                 and     #$01            ; is board square white or blk?
//>                 beq     pout25          ; white, print space
//>                 lda     #"*"            ; black, print *
//>                 db      $2c             ; used to skip over LDA #$20
//> POUT25          LDA     #$20            ; ASCII space
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - SPACE
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - SPACE
//> POUT3           INY                     ;
//>                   TYA                   ; get row number
//>                 AND     #$08            ; have we completed the row?
//>                 BEQ     POUT1           ; no, do next column
//>                 LDA     #"|"            ; yes, put the right edge on
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - |
//>                 jsr     pout12          ; print row number
//>                 JSR     POUT9           ; print CRLF
//>                 JSR     POUT5           ; print bottom edge of board
//>                 CLC                     ;
//>                 TYA                     ;
//>                 ADC     #$08            ; point y to beginning of next row
//>                 TAY                     ;
//>                 CPY     #$80            ; was that the last row?
//>                 BEQ     POUT8           ; yes, print the LED values
//>                 BNE     POUT1           ; no, do new row
//>
//> POUT4           LDA     REV             ; print piece's color & type
//>                 BEQ     POUT41          ;
//>                 LDA     cpl+16,X        ;
//>                 BNE     POUT42          ;
//> POUT41          LDA     cpl,x           ;
//> POUT42          JSR     syschout        ;
//>                 lda     cph,x           ;
//>                 jsr     syschout        ;
//>                 BNE     POUT3           ; branch always
//>
//> POUT5           TXA                     ; print "-----...-----<crlf>"
//>                 PHA
//>                 LDX     #$19
//>                 LDA     #"-"
//> POUT6           JSR     syschout        ; PRINT ONE ASCII CHR - "-"
//>                 DEX
//>                 BNE     POUT6
//>                 PLA
//>                 TAX
//>                 JSR     POUT9
//>                 RTS
//>
//> POUT8           jsr     pout10          ;
//>                 LDA     $FB
//>                 JSR     syshexout       ; PRINT 1 BYTE AS 2 HEX CHRS
//>                 LDA     #$20
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - SPACE
//>                 LDA     $FA
//>                 JSR     syshexout       ; PRINT 1 BYTE AS 2 HEX CHRS
//>                 LDA     #$20
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - SPACE
//>                 LDA     $F9
//>                 JSR     syshexout       ; PRINT 1 BYTE AS 2 HEX CHRS
//>
//> POUT9           LDA     #$0D
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - CR
//>                 LDA     #$0A
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - LF
//>                 RTS
//>
//> pout10          ldx     #$00            ; print the column labels
//> POUT11          lda     #$20            ; 00 01 02 03 ... 07 <CRLF>
//>                 jsr     syschout
//>                 txa
//>                 jsr     syshexout
//>                 INX
//>                 CPX     #$08
//>                 BNE     POUT11
//>                 BEQ     POUT9
//> POUT12          TYA
//>                 and     #$70
//>                 JSR     syshexout
//>                 rts
//>
//> Pout13          ldx     #$00            ; Print the copyright banner
//> Pout14          lda     banner,x
//>                 beq     POUT15
//>                 jsr     syschout
//>                 inx
//>                 bne     POUT14
//> POUT15          rts
//>

void pout() {
    putchar('\n');
    puts("MicroChess (c) 1996-2005 Peter Jennings, www.benlo.com");
    puts(" 00 01 02 03 04 05 06 07");
    int y = 0;
    puts("-------------------------");

    for (;;) {
        putchar('|');
        int x;
        for (x = 31; x >= 0; x--) {
            if (y == board[x])
                break;
        }

        if (x < 0) {                        // empty square
            char ch = (((y & 1) + ((y >> 4) & 1)) & 1) ? '*' : ' ';
            putchar(ch);
            putchar(ch);
        } else {
            putchar(rev ? cpl[x + 16] : cpl[x]);  // print piece's color & type
            putchar(cph[x]);
        }

        y++;
        if (y & 8) {                        // have we completed the row?
            printf("|%02X\n", (unsigned) (y & 0x70));  //jto: looks like a bug!
            puts("-------------------------");
            y += 8;                         // point y to beginning of next row
            if (y == 0x80)                  // was that the last row?
                break;                      // yes, print the LED values
        }
    }

    puts(" 00 01 02 03 04 05 06 07");
    printf("%02X %02X %02X\n",
           (unsigned) (uint8_t) bestp,
           (unsigned) (uint8_t) bestv,
           (unsigned) (uint8_t) bestm);
}

//> KIN             LDA     #"?"
//>                 JSR     syschout        ; PRINT ONE ASCII CHR - ?
//>                 JSR     syskin          ; GET A KEYSTROKE FROM SYSTEM
//>                 AND     #$4F            ; MASK 0-7, AND ALPHA'S
//>                 RTS

int kin() {
    putchar('?');
    int c = getchar();
    return c == EOF ? EOF : c & 0x4f;
}

#if 0
//> ;
//> ; 6551 I/O Support Routines
//> ;
//> ;
//> Init_6551      lda   #$1F               ; 19.2K/8/1
//>                sta   ACIActl            ; control reg
//>                lda   #$0B               ; N parity/echo off/rx int off/ dtr active low
//>                sta   ACIAcmd            ; command reg
//>                rts                      ; done
//> ;
//> ; input chr from ACIA1 (waiting)
//> ;
//> syskin         lda   ACIASta            ; Serial port status
//>                and   #$08               ; is recvr full
//>                beq   syskin             ; no char to get
//>                Lda   ACIAdat            ; get chr
//>                RTS                      ;
//> ;
//> ; output to OutPut Port
//> ;
//> syschout       PHA                      ; save registers
//> ACIA_Out1      lda   ACIASta            ; serial port status
//>                and   #$10               ; is tx buffer empty
//>                beq   ACIA_Out1          ; no
//>                PLA                      ; get chr
//>                sta   ACIAdat            ; put character to Port
//>                RTS                      ; done
//>
//> syshexout      PHA                     ;  prints AA hex digits
//>                LSR                     ;  MOVE UPPER NIBBLE TO LOWER
//>                LSR                     ;
//>                LSR                     ;
//>                LSR                     ;
//>                JSR   PrintDig          ;
//>                PLA                     ;
//> PrintDig       AND   #$0F              ;  prints A hex nibble (low 4 bits)
//>                PHY
//>                TAY                     ;
//>                LDA   Hexdigdata,Y      ;
//>                PLY
//>                jmp   syschout          ;
//>
//> Hexdigdata      asc     "0123456789ABCDEF"
#endif

//> banner          asc     "MicroChess (c) 1996-2005 Peter Jennings, www.benlo.com"
//>                 db      $0d, $0a, $00
//> cpl             asc     "WWWWWWWWWWWWWWWWBBBBBBBBBBBBBBBBWWWWWWWWWWWWWWWW"
//> cph             asc     "KQRRBBNNPPPPPPPPKQRRBBNNPPPPPPPP"
const char *cpl = "WWWWWWWWWWWWWWWWBBBBBBBBBBBBBBBBWWWWWWWWWWWWWWWW";
const char *cph = "KQRRBBNNPPPPPPPPKQRRBBNNPPPPPPPP";

//>                 db      $00
//> ;
//> ; end of added code
//> ;

//> ; BLOCK DATA
//>                 *= $1580

//> SETW            db      $03, $04, $00, $07, $02, $05, $01, $06
//>                 db      $10, $17, $11, $16, $12, $15, $14, $13
//>                 db      $73, $74, $70, $77, $72, $75, $71, $76
//>                 db      $60, $67, $61, $66, $62, $65, $64, $63
//>

// Initial positions of all the pieces.
const uint8_t setw[32] = {
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

//> MOVEX           db      $00, $F0, $FF, $01, $10, $11, $0F, $EF, $F1
//>                 db      $DF, $E1, $EE, $F2, $12, $0E, $1F, $21
//>

const uint8_t movex[17] = {
    0x00, 0xf0, 0xff, 0x01, 0x10, 0x11, 0x0f, 0xef, 0xf1,
    0xdf, 0xe1, 0xee, 0xf2, 0x12, 0x03, 0x1f, 0x21
};

//> POINTS          db      $0B, $0A, $06, $06, $04, $04, $04, $04
//>                 db      $02, $02, $02, $02, $02, $02, $02, $02
//>

const uint8_t points[16] = {
    11, 10, 6, 6, 4, 4, 4, 4,
    2, 2, 2, 2, 2, 2, 2, 2
};

//> OPNING          db      $99, $25, $0B, $25, $01, $00, $33, $25
//>                 db      $07, $36, $34, $0D, $34, $34, $0E, $52
//>                 db      $25, $0D, $45, $35, $04, $55, $22, $06
//>                 db      $43, $33, $0F, $CC
//>

/*
 * Microchess knows a 9-move standard canned opening.  This is the opening.
 * It's read by go(), and (like everything else in the program) the array is
 * read backwards.  A global variable 'omove' indexes into this array.
 *
 * The 0xcc at opning[27] refers to the initial state of the "black's last move" variable.
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
const uint8_t opning[28] = {
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


//> ;
//> ;
//> ; end of file
//> ;

/*
 * Turn the terminal's raw mode on or off.
 *
 * The code Daryl Rictor added so that Microchess could be played over a serial
 * port has the program responding to each keypress as soon as it happens.
 *
 * But on POSIX systems when the terminal is in the default (canonical) mode,
 * input is delivered to the process one line at a time. This function turns
 * off canonical mode, so each keystroke is delivered immediately: getchar()
 * returns as soon as you hit a key.
 *
 * (We also turn off the bits that cause the letters you type to be "echoed",
 * i.e. automatically displayed in your terminal even though the process didn't
 * print anything.)
 *
 * These terminal settings are per-terminal, *not* per-process, and the system
 * doesn't automatically revert our changes when we exit. This means if you
 * exit Microchess by hitting Ctrl+C, your terminal will be screwed up. Fix it
 * with the command `stty sane`.
 */
void change_terminal(bool raw) {
#ifdef HAVE_TERMIOS
    int flags = ECHO | ECHONL | ICANON;
    struct termios t;
    if (tcgetattr(STDIN_FILENO, &t) != 0) {
        perror("tcgetattr");
        exit(1);
    }
    if (raw)
        t.c_lflag &= ~flags;                // Turn off flags, enter raw mode.
    else
        t.c_lflag |= flags;                 // Turn flags back on before exiting.
    if (tcsetattr(STDIN_FILENO, TCSANOW, &t) != 0) {
        perror("tcsetattr");
        exit(1);
    }
#endif
}

int main() {
    change_terminal(true);
    chess();
    change_terminal(false);
    return 0;
}
