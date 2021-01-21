// *** chess.rs: this little game maybe you've heard of it ********************

use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::Range;
use std::str::FromStr;

use crate::minimax::Game;
use crate::minimax::best_move_with_depth_limit;


// *** Game state structs *****************************************************

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ChessColor { White, Black }

use ChessColor::*;

#[derive(Clone, Copy)]
pub struct Side {
    pawns: u64,
    knights: u64,
    bishops: u64,
    rooks: u64,
    king: u64,
    castle_k: bool,  // can still castle on king's side
    castle_q: bool   // can still castle on queen's side
}

#[derive(Clone, Copy)]
pub struct Chessboard {
    white: Side,
    black: Side,
    pub whose_turn: ChessColor,

    // en_passant is usually 0, but immediately after a pawn advances 2 spaces
    // from its initial position, en_passant is set to the position the pawn
    // skipped, the one an opposing pawn would move to in an en passant capture.
    en_passant: u64
}


// *** Utility methods

impl ChessColor {
    fn flip(self) -> ChessColor {
        match self {
            White => Black,
            Black => White
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum ChessPiece { King, Queen, Rook, Knight, Bishop, Pawn }

impl Side {
    fn all(&self) -> u64 {
        self.pawns | self.knights | self.bishops | self.rooks | self.king
    }

    fn piece_at(&self, bit: u64) -> Option<ChessPiece> {
        if self.pawns & bit != 0         { Some(ChessPiece::Pawn) }
        else if self.knights & bit != 0  { Some(ChessPiece::Knight) }
        else if self.bishops & bit != 0  { Some(if self.rooks & bit != 0 { ChessPiece::Queen }
                                                else { ChessPiece::Bishop }) }
        else if self.rooks & bit != 0    { Some(ChessPiece::Rook) }
        else if self.king == bit         { Some(ChessPiece::King) }
        else                             { None }
    }
}


// *** Printing chessboards ***************************************************

impl Chessboard {
    fn funny_mark_at(&self, col: u8, row: u8) -> char {
        let bit = 1u64 << (8 * row + col);
        match self.white.piece_at(bit) {
            Some(ChessPiece::King)   => '♔',
            Some(ChessPiece::Queen)  => '♕',
            Some(ChessPiece::Rook)   => '♖',
            Some(ChessPiece::Bishop) => '♗',
            Some(ChessPiece::Knight) => '♘',
            Some(ChessPiece::Pawn)   => '♙',
            None => match self.black.piece_at(bit) {
                Some(ChessPiece::King)   => '♚',
                Some(ChessPiece::Queen)  => '♛',
                Some(ChessPiece::Rook)   => '♜',
                Some(ChessPiece::Bishop) => '♝',
                Some(ChessPiece::Knight) => '♞',
                Some(ChessPiece::Pawn)   => '♟',
                None => '.'
            }
        }
    }
}        

fn concat<I: Iterator<Item=String>>(iter: I) -> String {
    let mut s = String::new();
    for x in iter {
        s.push_str(&x)
    }
    s
}

const RANGE : Range<u8> = Range { start: 0, end: 8 };
const COL_LETTERS : &'static [char] = &['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
const ROW_DIGITS  : &'static [char] = &['1', '2', '3', '4', '5', '6', '7', '8'];

impl Debug for Chessboard {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(&(
            concat(RANGE.rev().map(|r| {
                let mut s = String::new();
                s.push_str("    ");
                for c in RANGE {
                    s.push(self.funny_mark_at(c, r));
                    s.push(' ');
                }
                s.push_str("  ");
                s.push(ROW_DIGITS[r as usize]);
                s.push('\n');
                s
            })) + "\n    a b c d e f g h\n"))
    }
}


// *** Reading chessboards ****************************************************

fn file_to_int(file: char) -> i32 {
    file as i32 - 'a' as i32
}

pub fn fen_to_board(fen: &str) -> Chessboard {
    let mut words = fen.split(' ');
    let pieces_str = words.next().expect("fen_to_board: empty string");
    let whose_turn_str = words.next().expect("fen_to_board: missing whose-turn field");
    let castling_str = words.next().expect("fen_to_board: missing castling field");
    let en_passant_str = words.next().expect("fen_to_board: missing en-passant field");

    let whose_turn = if whose_turn_str == "w" { White }
                     else if whose_turn_str == "b" { Black }
                     else { panic!("unrecognized whose-turn string in fen: {}", whose_turn_str); };

    let en_passant =
        if en_passant_str == "-" {
            0
        } else if en_passant_str.len() != 1 {
            panic!("bad en-passant string in fen: {}", en_passant_str);
        } else {
            let pawn_row = match whose_turn {
                White => 0x00000100_00000000,
                Black => 0x00000000_00010000
            };
            pawn_row << file_to_int(en_passant_str.chars().next().expect("stupid"))
        };

    let mut result = Chessboard {
        white: Side {
            pawns:   0,
            knights: 0,
            bishops: 0,
            rooks:   0,
            king:    0,
            castle_k: castling_str.contains('K'),
            castle_q: castling_str.contains('Q')
        },
        black: Side {
            pawns:   0,
            knights: 0,
            bishops: 0,
            rooks:   0,
            king:    0,
            castle_k: castling_str.contains('k'),
            castle_q: castling_str.contains('q')
        },
        whose_turn: whose_turn,
        en_passant: en_passant
    };

    let mut i: u32 = 0;

    fn i_to_bit(i: u32) -> u64 {
        let file_mask = 7;     // the three least significant bits
        let rank_mask = 0x38;  // next three bits
        let file = i & file_mask;
        let reversed_rank_bits = 0x40 - (i & rank_mask);
        1u64 << (file | reversed_rank_bits)
    }

    for ch in pieces_str.chars() {
        let bit = i_to_bit(i);
        match ch {
            'P' => { result.white.pawns   |= bit; }
            'N' => { result.white.knights |= bit; }
            'B' => { result.white.bishops |= bit; }
            'R' => { result.white.rooks   |= bit; }
            'Q' => { result.white.bishops |= bit;
                     result.white.rooks   |= bit; }
            'K' => { result.white.king    |= bit; }
            'p' => { result.black.pawns   |= bit; }
            'n' => { result.black.knights |= bit; }
            'b' => { result.black.bishops |= bit; }
            'r' => { result.black.rooks   |= bit; }
            'q' => { result.black.bishops |= bit;
                     result.black.rooks   |= bit; }
            'k' => { result.black.king    |= bit; }
            '/' => { assert_eq!(i & 7, 0); }
            '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                i += ch as u32 - '0' as u32;
                continue;
            }
            other => { panic!("unexpected character in fen: {}", other); }
        }
        i += 1;
    }

    result
}


// *** Reading and writing moves **********************************************

#[derive(Copy, Clone, PartialEq)]
pub struct ChessMove {
   from: u64,
   to: u64,
   promote_to: Option<ChessPiece>
}

fn chess_move(from: u64, to: u64) -> ChessMove {
   ChessMove { from: from, to: to, promote_to: None }
}

const WHITE_CASTLES : &'static [(ChessMove, u64, u64)] = &[
    // white castling king's side
    (ChessMove { from: 0x00000000_00000010, to: 0x00000000_00000040, promote_to: None },
     0x00000000_00000060,
     0x00000000_00000020),
    // white castling queen's side
    (ChessMove { from: 0x00000000_00000010, to: 0x00000000_00000004, promote_to: None },
     0x00000000_0000000e,
     0x00000000_00000008)];

const BLACK_CASTLES : &'static [(ChessMove, u64, u64)] = &[
    // black castling king's side
    (ChessMove { from: 0x10000000_00000000, to: 0x40000000_00000000, promote_to: None },
     0x60000000_00000000,
     0x20000000_00000000),
    // black castling queen's side
    (ChessMove { from: 0x10000000_00000000, to: 0x04000000_00000000, promote_to: None },
     0x0e000000_00000000,
     0x08000000_00000000)];

fn log2_of_bit(b: u64) -> u32 {
    assert!(b.count_ones() == 1);
    63 - b.leading_zeros()
}

fn chars_to_str(chars: &[char]) -> String {
    let mut s = String::new();
    for &c in chars {
        s.push(c);
    }
    s
}

fn bit_to_str(b: u64) -> String {
    let square = log2_of_bit(b);
    let row = square / 8;
    let col = square % 8;
    chars_to_str(&[COL_LETTERS[col as usize], ROW_DIGITS[row as usize]])
}

impl Debug for ChessMove {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(&bit_to_str(self.from))?;
        f.write_str(&bit_to_str(self.to))?;
        match self.promote_to {
            None => Ok(()),
            Some(piece) => f.write_str(match piece {
                ChessPiece::Queen  => "q",
                ChessPiece::Knight => "n",
                ChessPiece::Rook   => "r",
                ChessPiece::Bishop => "b",
                _ => unreachable!()  // can't promote to King or Pawn
            })
        }
    }
}

pub fn chars_to_bit(c: char, r: char) -> Option<u64> {
    // TODO: make this code not die on overflow
    let uc = c as u32 - COL_LETTERS[0] as u32;
    let ur = r as u32 - ROW_DIGITS[0] as u32;
    if uc < 8 && ur < 8 {
        Some(1u64 << (ur * 8 + uc))
    } else {
        None
    }
}

pub struct ChessMoveParseError;

impl FromStr for ChessMove {
    type Err = ChessMoveParseError;

    fn from_str(s: &str) -> Result<ChessMove, ChessMoveParseError> {
        let v : Vec<char> = s.chars().collect();
        let n = v.len();
        if n < 4 || n > 5 {
            return Err(ChessMoveParseError);
        }
        match (chars_to_bit(v[0], v[1]), chars_to_bit(v[2], v[3])) {
            (Some(from), Some(to)) =>
                if n == 4 {
                    Ok(ChessMove { from: from, to: to, promote_to: None })
                } else {
                    let piece = match v[4] {
                        'q' => ChessPiece::Queen,
                        'n' => ChessPiece::Knight,
                        'r' => ChessPiece::Rook,
                        'b' => ChessPiece::Bishop,
                        _ => return Err(ChessMoveParseError)
                    };
                    Ok(ChessMove { from: from, to: to, promote_to: Some(piece) })
                },
            _ => Err(ChessMoveParseError)
        }
    }
}


// *** Rules of chess *********************************************************

#[derive(Copy, Clone)]
struct Biterator {
    bits: u64
}

impl Iterator for Biterator {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        if self.bits == 0 {
            None
        } else {
            let k = self.bits & !(self.bits - 1);
            self.bits = self.bits & !k;
            Some(k)
        }
    }
}

fn split_bits(b: u64) -> Biterator {
    Biterator {bits: b}
}

// ROOK_DIRS and friends are lists of pairs (shift, mask). 'shift' tells how
// much to shift a bit to move the corresponding piece in this direction.
// 'mask' tells whether a given piece is allowed to move this direction.
//
// For example, suppose you have a rook at a1, which is bit 0.  That rook's
// bit-pattern is 0x0000000000000001.  Bitwise AND that with each mask below to
// see which directions the rook can move. It can't move toward white or toward
// the queen's side, because it's already on the edge of the board. The other
// directions are ok, so 0x0000000000000001.rotate_left(1) and
// 0x0000000000000001.rotate_left(8) each produce new possible positions for the rook.

const KING_DIRS : [(u32, u64); 8] = [
    (64-8, 0xffffffff_ffffff00),  // S, toward white
    (64-1, 0xfefefefe_fefefefe),  // W, toward the queen's side, I think
    (   1, 0x7f7f7f7f_7f7f7f7f),  // E, toward the king's side
    (   8, 0x00ffffff_ffffffff),  // N, toward black
    (64-9, 0xfefefefe_fefefe00),  // SW
    (64-7, 0x7f7f7f7f_7f7f7f00),  // SE
    (   7, 0x00fefefe_fefefefe),  // NW
    (   9, 0x007f7f7f_7f7f7f7f)   // NE
];

const BLACK_PAWN_CAPTURE_DIRS : &'static [(u32, u64)] = &[
    KING_DIRS[4], KING_DIRS[5]
];
const WHITE_PAWN_CAPTURE_DIRS : &'static [(u32, u64)] = &[
    KING_DIRS[6], KING_DIRS[7]
];

const KNIGHT_DIRS : [(u32, u64); 8] = [
    (64-17, 0xfefefefe_fefe0000), // SSW
    (64-15, 0x7f7f7f7f_7f7f0000), // SSE
    (64-10, 0xfcfcfcfc_fcfcfc00), // WSW
    (64- 6, 0x3f3f3f3f_3f3f3f00), // ESE
    (    6, 0x00fcfcfc_fcfcfcfc), // WNW
    (   10, 0x003f3f3f_3f3f3f3f), // ENE
    (   15, 0x0000fefe_fefefefe), // NNW
    (   17, 0x00007f7f_7f7f7f7f)  // NNE
];

trait Dir {
    fn shift(b: u64) -> u64;
    fn smear(b: u64) -> u64;
    fn ignore(&self);
}

struct North;
impl Dir for North {
    fn shift(b: u64) -> u64 {
        b << 8
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | North::shift(b);
        let b2 = b1 | (b1 << 16);
        b2 | (b2 << 32)
    }

    fn ignore(&self) {}
}

struct South;
impl Dir for South {
    fn shift(b: u64) -> u64 {
        b >> 8
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | South::shift(b);
        let b2 = b1 | (b1 >> 16);
        b2 | (b2 >> 32)
    }

    fn ignore(&self) {}
}

struct East;
impl Dir for East {
    fn shift(b: u64) -> u64 {
        (b << 1) & 0xfefefefe_fefefefe
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | East::shift(b);
        let b2 = b1 | ((b1 << 2) & 0xfcfcfcfc_fcfcfcfc);
        b2 | ((b2 << 4) & 0xf0f0f0f0_f0f0f0f0)
    }

    fn ignore(&self) {}
}

struct West;
impl Dir for West {
    fn shift(b: u64) -> u64 {
        (b >> 1) & 0x7f7f7f7f_7f7f7f7f
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | West::shift(b);
        let b2 = b1 | ((b1 >> 2) & 0x3f3f3f3f_3f3f3f3f);
        b2 | ((b2 >> 4) & 0x0f0f0f0f_0f0f0f0f)
    }

    fn ignore(&self) {}
}

struct Northeast;
impl Dir for Northeast {
    fn shift(b: u64) -> u64 {
        (b << 9) & 0xfefefefe_fefefefe
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | Northeast::shift(b);
        let b2 = b1 | ((b1 << 18) & 0xfcfcfcfc_fcfcfcfc);
        b2 | ((b2 << 36) & 0xf0f0f0f0_f0f0f0f0)
    }

    fn ignore(&self) {}
}

struct Northwest;
impl Dir for Northwest {
    fn shift(b: u64) -> u64 {
        (b << 7) & 0x7f7f7f7f_7f7f7f7f
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | Northwest::shift(b);
        let b2 = b1 | ((b1 << 14) & 0x3f3f3f3f_3f3f3f3f);
        b2 | ((b2 << 28) & 0x0f0f0f0f_0f0f0f0f)
    }

    fn ignore(&self) {}
}

struct Southwest;
impl Dir for Southwest {
    fn shift(b: u64) -> u64 {
        (b >> 9) & 0x7f7f7f7f_7f7f7f7f
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | Southwest::shift(b);
        let b2 = b1 | ((b1 >> 18) & 0x3f3f3f3f_3f3f3f3f);
        b2 | ((b2 >> 36) & 0x0f0f0f0f_0f0f0f0f)
    }

    fn ignore(&self) {}
}

struct Southeast;
impl Dir for Southeast {
    fn shift(b: u64) -> u64 {
        (b >> 7) & 0xfefefefe_fefefefe
    }

    fn smear(b: u64) -> u64 {
        let b1 = b | Southeast::shift(b);
        let b2 = b1 | ((b1 >> 14) & 0xfcfcfcfc_fcfcfcfc);
        b2 | ((b2 >> 28) & 0xf0f0f0f0_f0f0f0f0)
    }

    fn ignore(&self) {}
}

/// Exhaustively test the shift and smear functions.
#[test]
fn shift_and_smear_functions() {
    type Dir = (&'static str, i8, i8, fn(u64) -> u64, fn(u64) -> u64);

    let dirs : [Dir; 8] = [
        ( "e",  1,  0, East::shift,      East::smear),
        ("ne",  1,  1, Northeast::shift, Northeast::smear),
        ( "n",  0,  1, North::shift,     North::smear),
        ("nw", -1,  1, Northwest::shift, Northwest::smear),
        ( "w", -1,  0, West::shift,      West::smear),
        ("sw", -1, -1, Southwest::shift, Southwest::smear),
        ( "s",  0, -1, South::shift,     South::smear),
        ("se",  1, -1, Southeast::shift, Southeast::smear)
    ];

    fn bit_to_coords(b: u64) -> (u8, u8) {
        let k = log2_of_bit(b) as u8;
        (k % 8, k / 8)
    }

    fn coords_to_bit(col: u8, row: u8) -> u64 {
        assert!(col < 8);
        assert!(row < 8);
        1 << (row * 8 + col)
    }

    fn slow_shift(b: u64, dx: i8, dy: i8) -> u64 {
        let (x, y) = bit_to_coords(b);
        let xx = (x as i32 + dx as i32) as u8;
        let yy = (y as i32 + dy as i32) as u8;
        if xx < 8 && yy < 8 {
            coords_to_bit(xx, yy)
        } else {
            0
        }
    }

    fn slow_smear(mut b: u64, dx: i8, dy: i8) -> u64 {
        let mut acc = 0;
        while b != 0 {
            acc |= b;
            b = slow_shift(b, dx, dy);
        }
        acc
    }

    for &(name, dx, dy, shift_fn, smear_fn) in &dirs {
        for i in (0..64) {
            let b = 1u64 << i;
            if shift_fn(b) != slow_shift(b, dx, dy) {
                panic!(String::new() +
                       "assertion failed: shift_" + name + "(1<<" + &i.to_string() + ") != slow_shift(1<<" +
                       &i.to_string() + ", " + &dx.to_string() + ", " + &dy.to_string() + "): " +
                       &shift_fn(b).to_string() + " != " + &slow_shift(b, dx, dy).to_string());
            }
            if smear_fn(b) != slow_smear(b, dx, dy) {
                panic!(String::new() +
                       "assertion failed: smear_" + name + "(1<<" + &i.to_string() + ") != slow_smear(1<<" +
                       &i.to_string() + ", " + &dx.to_string() + ", " + &dy.to_string() + "): " +
                       &smear_fn(b).to_string() + " != " + &slow_smear(b, dx, dy).to_string());
            }
        }
    }
}

fn moves_along_ray<D: Dir>(square: u64, friends: u64, enemies: u64) -> u64 {
    let ray = D::smear(D::shift(square));
    let friend_shadow = D::smear(friends & ray);
    let enemy_shadow = D::smear(D::shift(enemies & ray));
    ray & !(friend_shadow | enemy_shadow)
}

fn rook_move_bits(square: u64, friends: u64, enemies: u64) -> u64 {
    moves_along_ray::<East>(square, friends, enemies) |
    moves_along_ray::<North>(square, friends, enemies) |
    moves_along_ray::<West>(square, friends, enemies) |
    moves_along_ray::<South>(square, friends, enemies)
}

fn bishop_move_bits(square: u64, friends: u64, enemies: u64) -> u64 {
    moves_along_ray::<Northeast>(square, friends, enemies) |
    moves_along_ray::<Northwest>(square, friends, enemies) |
    moves_along_ray::<Southwest>(square, friends, enemies) |
    moves_along_ray::<Southeast>(square, friends, enemies)
}

fn square_is_attacked_on_ray<D: Dir>(square: u64, attackers: u64, others: u64) -> bool {
    let ray = D::smear(D::shift(square));
    let attacker_shadow = D::smear(attackers & ray);
    attacker_shadow != 0 &&  // pure optimization to avoid computing the shadow of others
        (attacker_shadow & !D::smear(others & ray)) != 0
}

fn square_is_attacked_by_rook(square: u64, rooks: u64, others: u64) -> bool {
    square_is_attacked_on_ray::<East>(square, rooks, others) ||
    square_is_attacked_on_ray::<North>(square, rooks, others) ||
    square_is_attacked_on_ray::<West>(square, rooks, others) ||
    square_is_attacked_on_ray::<South>(square, rooks, others)
}

fn square_is_attacked_by_bishop(square: u64, rooks: u64, others: u64) -> bool {
    square_is_attacked_on_ray::<Northeast>(square, rooks, others) ||
    square_is_attacked_on_ray::<Northwest>(square, rooks, others) ||
    square_is_attacked_on_ray::<Southwest>(square, rooks, others) ||
    square_is_attacked_on_ray::<Southeast>(square, rooks, others)
}

const KNIGHT_MASK : u64 = 0x04428000_00028440;

fn square_is_attacked_by_knight(square: u64, knights: u64) -> bool {
    KNIGHT_MASK.rotate_left(log2_of_bit(square)) & knights != 0 &&   // quick test with false positives
    KNIGHT_DIRS.iter().any(|&(shift_amount, mask)|
        square & (knights & mask).rotate_left(shift_amount as u32) != 0)
}

fn square_is_attacked_by_white_pawn(square: u64, white_pawns: u64) -> bool {
    square & ((white_pawns & 0x007f7f7f_7f7f7f7f) << 9) != 0
    || square & ((white_pawns & 0x00fefefe_fefefefe) << 7) != 0
}

fn square_is_attacked_by_black_pawn(square: u64, black_pawns: u64) -> bool {
    square & ((black_pawns & 0xfefefefe_fefefe00) >> 9) != 0
    || square & ((black_pawns & 0x7f7f7f7f_7f7f7f00) >> 7) != 0    
}

const KING_MASK : u64 = 0x83800000_00000382;

fn square_is_attacked_by_king(square: u64, king: u64) -> bool {
    KING_MASK.rotate_left(log2_of_bit(square)) & king != 0 &&  // quick test with false positives
    (square == East::shift(king) ||
     square == Northeast::shift(king) ||
     square == North::shift(king) ||
     square == Northwest::shift(king) ||
     square == West::shift(king) ||
     square == Southwest::shift(king) ||
     square == South::shift(king) ||
     square == Southeast::shift(king))
}

impl Chessboard {
    /// Given the board 'g' and a bit 'square', return true if any of 'color's
    /// pieces are attacking that square.
    ///
    /// This is an optimization of the original algorithm, which was (in Haskell):
    ///   let hypothetical = g {whose_turn = color}
    ///   in any (\(ChessMove _ to _ ) -> to == square) $ naiveMoves hypothetical
    ///
    fn square_is_threatened_by(&self, square: u64, color: ChessColor) -> bool {
        let (attackers, defenders, square_is_attacked_by_pawn) = match color {
            White => (&self.white, &self.black,
                      square_is_attacked_by_white_pawn(square, self.white.pawns)),
            Black => (&self.black, &self.white,
                      square_is_attacked_by_black_pawn(square, self.black.pawns))
        };
        let all_pieces = attackers.all() | defenders.all();

        square_is_attacked_by_pawn ||
            square_is_attacked_by_rook(square, attackers.rooks, all_pieces & !attackers.rooks) ||
            square_is_attacked_by_bishop(square, attackers.bishops, all_pieces & !attackers.bishops) ||
            square_is_attacked_by_knight(square, attackers.knights) ||
            square_is_attacked_by_king(square, attackers.king)
    }
}

const RANK_2: u64 = 0x00000000_0000ff00;  // white pawns start here
const RANK_7: u64 = 0x00ff0000_00000000;  // black pawns start here

const PROMOTE_PIECES : [ChessPiece; 4] = [
    ChessPiece::Queen,
    ChessPiece::Knight,
    ChessPiece::Rook,
    ChessPiece::Bishop
];

impl Chessboard {
    /// Return the list of all moves, without eliminating moves that leave
    /// the current player's king in check. Moves where a piece would capture
    /// an opposing king are also included (indeed we rely on this).
    fn naive_moves(&self) -> Vec<ChessMove> {
        let mut moves = Vec::new();

        let friendly;
        let enemy_pieces;
        let pawn_home_row;
        let pawn_promote_row;
        let pawn_shift_amount;
        let pawn_capture_dirs;
        let castles;
        match self.whose_turn {
            White => {
                friendly = &self.white;
                enemy_pieces = self.black.all();
                pawn_home_row = RANK_2;
                pawn_promote_row = RANK_7;
                pawn_shift_amount = 8u32;
                pawn_capture_dirs = WHITE_PAWN_CAPTURE_DIRS;
                castles = WHITE_CASTLES;
            }
            Black => {
                friendly = &self.black;
                enemy_pieces = self.white.all();
                pawn_home_row = RANK_7;
                pawn_promote_row = RANK_2;
                pawn_shift_amount = 64u32 - 8u32;
                pawn_capture_dirs = BLACK_PAWN_CAPTURE_DIRS;
                castles = BLACK_CASTLES;
            }
        }

        if friendly.king == 0 {
            return moves;
        }

        let friendly_pieces = friendly.all();
        let all_pieces = friendly_pieces | enemy_pieces;

        // Try a single move. The caller must ensure that the piece will land on
        // the board when we move it in this direction; but we still need to check
        // that we are not trying to move on top of one of our own pieces. If this
        // is a legal move, add it to v.
        fn try_single_move(shift_amount: u32, b: u64, friendly_pieces: u64, moves: &mut Vec<ChessMove>) {
            let after = b.rotate_left(shift_amount);
            if after & friendly_pieces == 0 {
                moves.push(chess_move(b, after));
            }
        };

        for from in split_bits(friendly.rooks) {
            for to in split_bits(rook_move_bits(from, friendly_pieces, enemy_pieces)) {
                moves.push(chess_move(from, to));
            }
        }

        for from in split_bits(friendly.bishops) {
            for to in split_bits(bishop_move_bits(from, friendly_pieces, enemy_pieces)) {
                moves.push(chess_move(from, to));
            }
        }

        for &(shift_amount, mask) in &KNIGHT_DIRS {
            for start_square in split_bits(friendly.knights & mask) {
                try_single_move(shift_amount, start_square, friendly_pieces, &mut moves);
            }
        }

        for &(shift_amount, mask) in &KING_DIRS {
            if friendly.king & mask != 0 {
                try_single_move(shift_amount, friendly.king, friendly_pieces, &mut moves);
            }
        }

        fn add_pawn_move(from: u64, to: u64, promote: bool, moves: &mut Vec<ChessMove>) {
            if promote {
                for &piece in &PROMOTE_PIECES {
                    moves.push(ChessMove {from: from, to: to, promote_to: Some(piece)});
                }
            } else {
                moves.push(ChessMove {from: from, to: to, promote_to: None});
            }
        }

        // Pawn moves.
        let enemy_pieces_plus_en_passant = enemy_pieces | self.en_passant;
        for from in split_bits(friendly.pawns) {
            let promote = from & pawn_promote_row != 0;

            // Forward moves.
            let dest1 = from.rotate_left(pawn_shift_amount);
            if all_pieces & dest1 == 0 {
                add_pawn_move(from, dest1, promote, &mut moves);
                if from & pawn_home_row != 0 {
                    let dest2 = dest1.rotate_left(pawn_shift_amount);
                    if all_pieces & dest2 == 0 {
                        add_pawn_move(from, dest2, false, &mut moves);
                    }
                }
            }

            // Captures.
            for &(shift_amount, mask) in pawn_capture_dirs {
                if from & mask != 0 {
                    let dest = from.rotate_left(shift_amount);
                    if dest & enemy_pieces_plus_en_passant != 0 {
                        add_pawn_move(from, dest, promote, &mut moves);
                    }
                }
            }
        }

        fn add_castling_move_if_legal(game: &Chessboard,
                                      all_pieces: u64,
                                      castle : &(ChessMove, u64, u64),
                                      moves: &mut Vec<ChessMove>) {
            if all_pieces & castle.1 == 0 &&
                    !game.square_is_threatened_by(castle.2, game.whose_turn.flip()) {
                moves.push(castle.0);
            }
        }
        if friendly.castle_k {
            add_castling_move_if_legal(&self, all_pieces, &castles[0], &mut moves);
        }
        if friendly.castle_q {
            add_castling_move_if_legal(&self, all_pieces, &castles[1], &mut moves);
        }

        moves
    }

    fn legal_moves(&self) -> Vec<ChessMove> {
        self.naive_moves().iter()
            .filter(|m| !self.move_leaves_self_in_check(**m))
            .map(|p| *p)
            .collect()
    }

    fn move_leaves_self_in_check(&self, m: ChessMove) -> bool {
        // Suppose it's white to play. Then m is a move by white, and g1 is the board
        // after that move. In g1 it is black's turn. We want to know if white's king
        // is threatened by black's pieces.
        let g1 = self.apply_move(m);
        let me = self.whose_turn;
        let my_king = match me {
            White => g1.white.king,
            Black => g1.black.king
        };
        g1.square_is_threatened_by(my_king, me.flip())
    }

    fn apply_move_impl(&self, m: ChessMove) -> Chessboard {
        let ChessMove{from: from_bit, to: to_bit, promote_to: promote} = m;

        fn apply_move_to_bitboard(from_bit: u64, to_bit: u64, bits: u64) -> u64 {
            if bits & from_bit == 0 {
                bits
            } else {
                (bits & !from_bit) | to_bit
            }
        }

        let (friends, enemies, forward_shift, castle_k_spots, castle_q_spots) =
            match self.whose_turn {
                White => (&self.white, &self.black,    8, 0x00000000_00000090, 0x00000000_00000011),
                Black => (&self.black, &self.white, 64-8, 0x90000000_00000000, 0x11000000_00000000)
            };

        // Remove an enemy piece if capturing.
        let mask = !to_bit;
        let mut enemies_mod = Side {
            pawns:   enemies.pawns   & mask,
            knights: enemies.knights & mask,
            bishops: enemies.bishops & mask,
            rooks:   enemies.rooks   & mask,
            king:    enemies.king,
            castle_k: enemies.castle_k,
            castle_q: enemies.castle_q
        };

        // En passant capture.
        if to_bit == self.en_passant && friends.pawns & from_bit != 0 {
            enemies_mod.pawns &= !(to_bit.rotate_right(forward_shift));
        }

        // Move piece.
        let mut friends_mod = Side {
            pawns:   apply_move_to_bitboard(from_bit, to_bit, friends.pawns),
            knights: apply_move_to_bitboard(from_bit, to_bit, friends.knights),
            bishops: apply_move_to_bitboard(from_bit, to_bit, friends.bishops),
            rooks:   apply_move_to_bitboard(from_bit, to_bit, friends.rooks),
            king:    apply_move_to_bitboard(from_bit, to_bit, friends.king),
            castle_k: friends.castle_k && from_bit & castle_k_spots == 0,
            castle_q: friends.castle_q && from_bit & castle_q_spots == 0
        };

        // Finish castling.
        if from_bit == friends.king {
            if to_bit == from_bit << 2 {
                friends_mod.rooks = (friends_mod.rooks & !(from_bit << 3)) | (from_bit << 1);
            } else if to_bit == from_bit >> 2 {
                friends_mod.rooks = (friends_mod.rooks & !(from_bit >> 4)) | (from_bit >> 1);
            }
        }

        // Apply promotion.
        match promote {
            None => (),
            Some(piece) => {
                friends_mod.pawns &= !to_bit;
                match piece {
                    ChessPiece::Queen  => { friends_mod.bishops |= to_bit;
                                            friends_mod.rooks   |= to_bit; },
                    ChessPiece::Knight => { friends_mod.knights |= to_bit; },
                    ChessPiece::Bishop => { friends_mod.bishops |= to_bit; },
                    ChessPiece::Rook   => { friends_mod.rooks   |= to_bit; },
                    _ => unreachable!("bad promotion")
                }
            }
        };

        let en_passant_mod =
            if from_bit & friends.pawns != 0 && to_bit == from_bit.rotate_left(2 * forward_shift) {
                from_bit.rotate_left(forward_shift)
            } else {
                0
            };

        match self.whose_turn {
            White => Chessboard {
                white: friends_mod, black: enemies_mod, whose_turn: Black,
                en_passant: en_passant_mod
            },
            Black => Chessboard {
                white: enemies_mod, black: friends_mod, whose_turn: White,
                en_passant: en_passant_mod
            }
        }
    }
}

const STARTING_BOARD : Chessboard = Chessboard {
    black: Side {
        pawns:   0x00ff0000_00000000,
        knights: 0x42000000_00000000,
        bishops: 0x2c000000_00000000,
        rooks:   0x89000000_00000000,
        king:    0x10000000_00000000,
        castle_k: true,
        castle_q: true
    },
    white: Side {
        pawns:   0x00000000_0000ff00,
        knights: 0x00000000_00000042,
        bishops: 0x00000000_0000002c,
        rooks:   0x00000000_00000089,
        king:    0x00000000_00000010,
        castle_k: true,
        castle_q: true
    },
    whose_turn: White,
    en_passant: 0
};

impl Game for Chessboard {
    type Move = ChessMove;

    fn start() -> Chessboard { STARTING_BOARD.clone() }

    fn moves(&self) -> Vec<ChessMove> {
        self.legal_moves()
    }

    fn apply_move(&self, m: ChessMove) -> Chessboard {
        self.apply_move_impl(m)
    }

    fn score_finished_game(&self) -> f64 {
        let side = match self.whose_turn {
            White => self.white,
            Black => self.black
        };
        if self.square_is_threatened_by(side.king, self.whose_turn.flip()) { 1.0 } else { 0.0 }
    }
}


// *** Chess AI ***************************************************************

/*
    Some history:

    // The first heuristic I attempted was this amazingly bad one:
    fn heuristic0(_: &Chessboard) -> f64 { 0.0 }

    // Then I tried this almost-as-bad heuristic: just count pieces :)
    fn heuristic1(board: &Chessboard) -> f64 {
        let diff = 0.05 * (board.white.all().count_ones() as f64 -
                           board.black.all().count_ones() as f64);
        match board.whose_turn {
            White => diff,
            Black => -diff
        }
    }


    // Next I gave all the pieces values.
    fn heuristic2(board: &Chessboard) -> f64 {
        let diff = 0.00001 * material_advantage_for_white(board) as f64;
        match board.whose_turn {
            White => -diff,
            Black => diff
        }
    }

    // Next, for each piece, give its side a small bonus for the number of squares it
    // attacks, and for each friendly piece it protects.
    fn heuristic3(board: &Chessboard) -> f64 {
        let diff = 0.00000001 * (1000 * material_advantage_for_white(board) +
                                 mobility_advantage_for_white(board)) as f64;
        match board.whose_turn {
            White => -diff,
            Black => diff
        }
    }
*/

fn material_advantage_for_white(board: &Chessboard) -> i32 {
    fn total(side: &Side) -> i32 {
        // The scores here are from Wikipedia, which lists a dozen or more
        // scoring systems to choose from. This one is credited to Hans Berliner.
        //
        // This code is complicated slightly by our trick of representing a queen
        // as a bishop and a rook. For speed, we do not bother eliminating queens
        // when counting the number of bishops/rooks, but rather count queens as
        // both bishops and rooks, and then add a bit *more*...
        100 * side.pawns.count_ones() as i32 +
        320 * side.knights.count_ones() as i32 +
        333 * side.bishops.count_ones() as i32 +
        510 * side.rooks.count_ones() as i32 +
         37 * (side.bishops & side.rooks).count_ones() as i32
    }

    return total(&board.white) - total(&board.black);
}

fn sum<I: Iterator<Item=u32>>(i: I) -> u32 {
    let mut total = 0;
    for v in i {
        total += v;
    }
    total
}

fn mobility_advantage_for_white(board: &Chessboard) -> i32 {
    fn moves_and_protects_along_ray<D: Dir>(all_pieces: u64, square: u64) -> u64 {
        let ray = D::smear(D::shift(square));
        let shadow = D::smear(D::shift(all_pieces & ray));
        ray & !shadow
    }

    let all_pieces = board.white.all() | board.black.all();

    let mobility = |side: &Side| -> i32 {
        let rook_mobility = sum(split_bits(side.rooks).map(|square| {
            (moves_and_protects_along_ray::<East>(all_pieces, square) |
             moves_and_protects_along_ray::<North>(all_pieces, square) |
             moves_and_protects_along_ray::<West>(all_pieces, square) |
             moves_and_protects_along_ray::<South>(all_pieces, square)).count_ones()
        }));

        let bishop_mobility = sum(split_bits(side.bishops).map(|square| {
            (moves_and_protects_along_ray::<Northeast>(all_pieces, square) |
             moves_and_protects_along_ray::<Northwest>(all_pieces, square) |
             moves_and_protects_along_ray::<Southwest>(all_pieces, square) |
             moves_and_protects_along_ray::<Southeast>(all_pieces, square)).count_ones()
        }));

        let knight_mobility = sum(split_bits(side.knights).map(|square| {
            if square & 0xffc38181_8181c3ff != 0 {              // 2 3 4 4 4 4 3 2
                if square & 0x81000000_00000081 != 0 {          // 3 4 6 6 6 6 4 3
                    2                                           // 4 6 8 8 8 8 6 4
                } else if square & 0x42810000_00008142 != 0 {   // 4 6 8 8 8 8 6 4
                    3                                           // 4 6 8 8 8 8 6 4
                } else {                                        // 4 6 8 8 8 8 6 4
                    4                                           // 3 4 6 6 6 6 4 3
                }                                               // 2 3 4 4 4 4 3 2
            } else if square & 0x00003c3c_3c3c0000 != 0 {
                8
            } else {
                6
            }
        }));

        (rook_mobility + bishop_mobility + knight_mobility) as i32
    };
    mobility(&board.white) - mobility(&board.black)
}

fn pawn_advantage_for_white(board: &Chessboard) -> i32 {
    let w_pawns = board.white.pawns;
    let b_pawns = board.black.pawns;
    let all_pieces = board.white.all() | board.black.all();
    let white_pawn_bonus =
        (all_pieces & ((w_pawns & 0x00fefefe_fefefefe) << 7)).count_ones() +    // NW
        (all_pieces & ((w_pawns & 0x007f7f7f_7f7f7f7f) << 9)).count_ones();     // NE
    let black_pawn_bonus =
        (all_pieces & ((b_pawns & 0x7f7f7f7f_7f7f7f00) >> 7)).count_ones() +    // SE
        (all_pieces & ((b_pawns & 0xfefefefe_fefefe00) >> 9)).count_ones();     // SW
    white_pawn_bonus as i32 - black_pawn_bonus as i32
}

// Now, add some points for pawns attacking and defending other pieces.
fn heuristic(board: &Chessboard) -> f64 {
    let diff = 0.00000001 * (1000 * material_advantage_for_white(board) +
                                3 * mobility_advantage_for_white(board) +
                                2 * pawn_advantage_for_white(board)) as f64;
    match board.whose_turn {
        White => -diff,
        Black => diff
    }
}

/*
presortMoves g moves = sortBy (compare `on` captureStrength) moves
  where
    enemySide = (case whoseTurn g of {Black -> black; White -> white}) g
    on binop key a b = key a `binop` key b
    captureStrength (ChessMove _ dest _) =
      if rooks enemySide .&. dest /= 0
      then if bishops enemySide .&. dest /= 0
           then 1 -- Sort capturing a queen to the head of the list.
           else 2
      else if knights enemySide .&. dest /= 0 ||
              bishops enemySide .&. dest /= 0
           then 3
           else if pawns enemySide .&. dest /= 0
                then 4
                else 5  -- Not capturing anything. Sort to end of list.

chessAI = bestMoveWithPresortAndDepthLimit presortMoves heuristic 2
*/

pub fn ai(board: &Chessboard) -> ChessMove {
    best_move_with_depth_limit(&heuristic, 2, board)
}
