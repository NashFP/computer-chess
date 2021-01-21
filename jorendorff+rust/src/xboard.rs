// *** xboard.rs: Support for GNU XBoard **************************************

use std::io::prelude::*;
use std::str::FromStr;
use crate::chess::*;
use crate::chess::ChessColor::*;
use crate::minimax::Game;

enum MoveResult {
    MoveError(String),
    GameOver(String),
    Continue(Chessboard)
}
use MoveResult::*;

fn try_move(board: &Chessboard, str: &str) -> MoveResult {
    let m = match ChessMove::from_str(str) {
        Err(_) => { return MoveError(format!("unrecognized move: {}", str)); },
        Ok(m) => m
    };
    if !board.moves().contains(&m) {
        return MoveError(format!("illegal move: {:?}", m));
    }
    let result = board.apply_move(m);
    if result.moves().is_empty() {
        GameOver(if result.score_finished_game() == 0.0 {
                     "1/2-1/2 {stalemate}".to_string()
                 } else if result.whose_turn == Black {
                     "1-0 {White mates}".to_string()
                 } else {
                     "0-1 {Black mates}".to_string()
                 })
    } else {
        Continue(result)
    }
}

const IGNORED_COMMANDS : [&'static str; 31] = [
  "xboard", "accepted", "rejected", "variant", "random", "white", "black", "level",
  "st", "sd", "nps", "time", "otim", "?", "ping", "draw", "result", "edit", "hint", "bk", "undo",
  "remove", "hard", "easy", "post", "nopost", "analyze", "name", "rating", "computer", "option"];

struct XBoard<F: Fn(&Chessboard) -> ChessMove> {
    ai: F,
    board: Chessboard,
    force_mode: bool,
    log: std::fs::File,
}

impl<F: Fn(&Chessboard) -> ChessMove> XBoard<F> {
    fn say(&mut self, s: &str) -> std::io::Result<()> {
        writeln!(&mut self.log, "{}", s)?;
        self.log.flush()?;
        Ok(())
    }

    fn respond(&mut self, s: &str) -> std::io::Result<()> {
        println!("{}", s);
        std::io::stdout().flush()?;
        writeln!(&mut self.log, "<-- {}", s)?;
        self.log.flush()?;
        Ok(())
    }

    fn go(&mut self) -> std::io::Result<()> {
        let m = (self.ai)(&self.board);
        self.respond(&format!("move {:?}", m))?;
        self.board = self.board.apply_move(m);
        Ok(())
    }

    fn new(ai: F) -> std::io::Result<XBoard<F>> {
        Ok(XBoard {
            ai: ai,
            board: Chessboard::start(),
            force_mode: true,
            log: std::fs::OpenOptions::new().write(true).create(true).open("xboard-input.txt")?,
        })
    }

    fn play(&mut self) -> std::io::Result<()> {
        loop {
            let mut this_line = String::new();
            std::io::stdin().read_line(&mut this_line)?;

            this_line = this_line.trim_matches('\n').to_string();

            let command = match this_line.split(' ').next() {
                None => return Err(std::io::Error::new(std::io::ErrorKind::Other, "missing command")),
                Some(command) => command
            };

            self.say(&format!("--> {}", this_line))?;

            let arguments =
                if command.len() + 1 >= this_line.len() {
                    ""
                } else {
                    &this_line[command.len() + 1..]
                };

            if IGNORED_COMMANDS.contains(&command) {
                // do nothing
            } else if command == "new" {
                self.board = Chessboard::start();
                self.force_mode = false;
            } else if command == "protover" {
                self.respond("feature variants=\"normal\" usermove=1 draw=0 analyze=0 colors=0 setboard=1 sigint=0 done=1")?;
            } else if command == "quit" {
                return Ok(());
            } else if command == "force" {
                self.force_mode = true;
            } else if command == "setboard" {
                self.say(&arguments)?;
                self.board = fen_to_board(arguments);
                let s = format!("{:?}", self.board);
                self.say(&s)?;
            } else if command == "go" {
                self.force_mode = false;
                self.go()?;
                let s = format!("{:?}", self.board);
                self.say(&s)?;
            } else if command == "usermove" {
                match try_move(&self.board, arguments) {
                    MoveError(msg) => self.respond(&msg)?,
                    GameOver(msg) => self.respond(&msg)?,
                    Continue(board) => {
                        self.board = board;
                        if !self.force_mode {
                            self.go()?;
                        }
                    }
                }
                let s = format!("{:?}", self.board);
                self.say(&s)?;
            } else {
                self.respond(&format!("Error: (unknown command): {}", this_line))?;
            }
        }
    }
}

pub fn play_xboard<F: Fn(&Chessboard) -> ChessMove>(ai: F) -> std::io::Result<()> {
    let mut state = XBoard::<F>::new(ai)?;
    state.play()
}
