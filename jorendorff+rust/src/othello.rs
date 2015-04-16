// *** othello.rs: the game of Othello ****************************************

use std;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::Add;
use std::str::FromStr;
use minimax::Game;
use minimax::best_move_with_depth_limit;

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
enum Square { Empty, Black, White }

fn flip(sq: Square) -> Square {
    match sq {
        Square::Black => Square::White,
        Square::White => Square::Black,
        Square::Empty => Square::Empty
    }
}

const SIZE : usize = 8;

#[derive(Clone, Copy, PartialEq)]
pub enum OthelloMove {
    Pass,
    MoveAt(i32, i32)
}

const ROW_DIGITS : &'static str = "12345678";
const COL_LETTERS : &'static str = "abcdefgh";

impl Debug for OthelloMove {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            OthelloMove::Pass => f.write_str("pass"),
            OthelloMove::MoveAt(x, y) => f.write_str(&(COL_LETTERS[x as usize .. x as usize + 1].to_string() +
                                                       &ROW_DIGITS[y as usize .. y as usize + 1]))
        }
    }
}

pub struct OthelloMoveParseError;

fn index_of(s: &str, c: char) -> Option<usize> {
    for (i, sch) in s.chars().enumerate() {
        if sch == c {
            return Some(i);
        }
    }
    None
}


impl FromStr for OthelloMove {
    type Err = OthelloMoveParseError;
    fn from_str(s: &str) -> Result<OthelloMove, OthelloMoveParseError> {
        if s == "pass" {
            Ok(OthelloMove::Pass)
        } else {
            let mut it = s.chars();
            match it.next() {
                None => Err(OthelloMoveParseError),
                Some(c0) => match it.next() {
                    None => Err(OthelloMoveParseError),
                    Some(c1) =>
                        match (index_of(COL_LETTERS, c0), index_of(ROW_DIGITS, c1)) {
                            (Some(x), Some(y)) => Ok(OthelloMove::MoveAt(x as i32, y as i32)),
                            _ => Err(OthelloMoveParseError)
                        }
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Othello {
    board: [[Square; SIZE]; SIZE],
    turn: Square
}

impl Debug for Othello {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let mut buf = String::new();
        for y in (0 .. SIZE) {
            for x in (0 .. SIZE) {
                buf.push(match self.board[y][x] {
                    Square::Black => 'X',
                    Square::White => 'O',
                    Square::Empty => '.'
                });
                buf.push(' ');
            }
            buf.push(' ');
            buf.push_str(&ROW_DIGITS[y .. y + 1]);
            buf.push('\n');
        }

        for x in (0 .. SIZE) {
            buf.push_str(&COL_LETTERS[x .. x + 1]);
            buf.push(' ');
        }
        buf.push('\n');

        f.write_str(&buf)
    }
}

const DIRS : [(i32, i32); 8] = [
    (-1, -1), (-1,  0), (-1,  1),
    ( 0, -1),           ( 0,  1),
    ( 1, -1), ( 1,  0), ( 1,  1)
];

impl Othello {
    fn append_captures_along_ray(&self, x: i32, y: i32, dx: i32, dy: i32, v: &mut Vec<(i32, i32)>) {
        let me = self.turn;

        let mut cx = x + dx;
        let mut cy = y + dy;
        let v_len_start = v.len();

        loop {
            if cx < 0 || cx >= SIZE as i32 || cy < 0 || cy >= SIZE as i32 {
                v.truncate(v_len_start);
                return;
            }
            if self.board[cy as usize][cx as usize] == me {
                return;
            }
            if self.board[cy as usize][cx as usize] == Square::Empty {
                v.truncate(v_len_start);
                return;
            }
            v.push((cx, cy));
            cx += dx;
            cy += dy;
        }
    }

    fn capture_indexes(&self, x: i32, y: i32) -> Vec<(i32, i32)> {
        let mut v = Vec::new();
        for &(dx, dy) in &DIRS {
            self.append_captures_along_ray(x, y, dx, dy, &mut v);
        }
        v
    }

    fn can_capture_along_ray(&self, (x, y): (i32, i32), (dx, dy): (i32, i32)) -> bool {
        let me = self.turn;
        let mut cx = x + dx;
        let mut cy = y + dy;

        while cx >= 0 && cx < SIZE as i32 && cy >= 0 && cy < SIZE as i32 {
            let sq = self.board[cy as usize][cx as usize];
            if sq == me {
                return (cx, cy) != (x + dx, y + dy);
            }
            if sq == Square::Empty {
                return false;
            }
            cx += dx;
            cy += dy;
        }
        false
    }

    fn can_capture_at(&self, point: (i32, i32)) -> bool {
        DIRS.iter().any(|&dir| self.can_capture_along_ray(point, dir))
    }

    fn capturing_moves(&self) -> Vec<OthelloMove> {
        let mut v = Vec::new();
        for y in (0..SIZE as i32) {
            for x in (0..SIZE as i32) {
                if self.board[y as usize][x as usize] == Square::Empty &&
                        self.can_capture_at((x, y)) {
                    v.push(OthelloMove::MoveAt(x, y));
                }
            }
        }
        v
    }

    fn counts(&self) -> (i32, i32) {
        let you = self.turn;
        let me = flip(you);
        let mut yours = 0i32;
        let mut mine = 0i32;
        for y in (0..SIZE) {
            for x in (0..SIZE) {
                let sq = self.board[y][x];
                if      sq == you { yours += 1; }
                else if sq == me  { mine += 1; }
            }
        }
        (yours, mine)
    }
}

impl Game for Othello {
    type Move = OthelloMove;

    fn start() -> Othello {
        let mut st = Othello {
            board: [[Square::Empty; SIZE]; SIZE],
            turn: Square::Black
        };
        let m = SIZE / 2 - 1;
        st.board[m    ][m    ] = Square::Black;
        st.board[m    ][m + 1] = Square::White;
        st.board[m + 1][m    ] = Square::White;
        st.board[m + 1][m + 1] = Square::Black;
        st
    }

    fn moves(&self) -> Vec<OthelloMove> {
        let caps = self.capturing_moves();
        if caps.is_empty() {
            let replies = self.apply_move(OthelloMove::Pass).capturing_moves();
            if replies.is_empty() {
                // neither player has any moves
                vec![]
            } else {
                // other player has a move; this player must pass
                vec![OthelloMove::Pass]
            }
        } else {
            caps
        }
    }

    fn apply_move(&self, m: OthelloMove) -> Othello {
        match m {
            OthelloMove::Pass => self.clone(),
            OthelloMove::MoveAt(x, y) => {
                let me = self.turn;
                let you = flip(me);

                let v = self.capture_indexes(x, y);
                let mut result = self.clone();
                assert_eq!(result.board[y as usize][x as usize], Square::Empty);
                result.board[y as usize][x as usize] = me;
                for (x, y) in v {
                    assert_eq!(result.board[y as usize][x as usize], you);
                    result.board[y as usize][x as usize] = me;
                }
                result.turn = you;
                result
            }
        }
    }

    fn score_finished_game(&self) -> f64 {
        let (your_score, my_score) = self.counts();
        (my_score - your_score) as f64
    }
}


// *** AI *********************************************************************

const RUBRIC : [[i32; SIZE]; SIZE] = [
    [15, -2,  3,  3,  3,  3, -2, 15],
    [-2, -2, -1, -1, -1, -1, -2, -2],
    [ 3, -1,  1,  1,  1,  1, -1,  3],
    [ 3, -1,  1,  1,  1,  1, -1,  3],
    [ 3, -1,  1,  1,  1,  1, -1,  3],
    [ 3, -1,  1,  1,  1,  1, -1,  3],
    [-2, -2, -1, -1, -1, -1, -2, -2],
    [15, -2,  3,  3,  3,  3, -2, 15]];

fn sum<T: Add<T, Output=T>, I: Iterator<Item=T>>(start: T, iter: I) -> T {
    let mut total = start;
    for v in iter {
        total = total + v;
    }
    total
}    

fn heuristic(game: &Othello) -> f64 {
    let you = game.turn;
    let (yours, mine) = game.counts();
    let delta = mine - yours;
    if yours + mine < 15 {
        // opening: minimize
        -0.0025 * (delta as f64)
    } else if yours + mine < 45 {
        // midgame: play positionally
        0.005 * (sum(0.0, game.board.iter().zip(RUBRIC.iter()).map(|(board_row, rubric_row)| {
            sum(0, board_row.iter().zip(rubric_row.iter()).map(|(sq, &value)| {
                if *sq == Square::Empty  {      0 }
                else if *sq == you       { -value }
                else                     {  value }
            })) as f64
       })))
    } else {
        // endgame: pure greed
        0.01 * (delta as f64)
    }
}

pub fn smarty_pants_ai(board: &Othello) -> OthelloMove {
    best_move_with_depth_limit(&heuristic, 3, board)
}
