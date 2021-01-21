#![warn(rust_2018_idioms)]

mod minimax;
mod interactive_play;
mod pennies;
mod fifteen;
mod othello;
mod chess;
mod xboard;

use crate::{
    interactive_play::play_human_vs_computer,
    xboard::play_xboard,
    minimax::Game,
    minimax::best_move,
    pennies::Pennies,
    fifteen::Fifteen,
    othello::Othello,
    chess::Chessboard,
};

#[derive(PartialEq)]
enum GameId { Pennies, Fifteen, Othello, Chess }

fn main() {
    let mut selected_game: Option<GameId> = None;
    let mut xboard = false;

    {
        let mut lets_play = |g| {
            if selected_game.is_some() {
                panic!("multiple game options on command line");
            }
            selected_game = Some(g);
        };

        for arg in std::env::args() {
            if arg == "--pennies" {
                lets_play(GameId::Pennies);
            } else if arg == "--fifteen" {
                lets_play(GameId::Fifteen);
            } else if arg == "--othello" {
                lets_play(GameId::Othello);
            } else if arg == "--chess" {
                lets_play(GameId::Chess);
            } else if arg == "--xboard" {
                xboard = true;
            }
        }
    }

    let game = selected_game.unwrap_or(GameId::Chess);

    if xboard {
        if game != GameId::Chess {
            panic!("--xboard is for chess and can't be used with other games");
        }
        play_xboard(chess::ai).unwrap()
    } else {
        match game {
            GameId::Pennies => play_human_vs_computer(best_move::<Pennies>, Pennies::start()),
            GameId::Fifteen => play_human_vs_computer(best_move::<Fifteen>, Fifteen::start()),
            GameId::Othello => play_human_vs_computer(othello::smarty_pants_ai, Othello::start()),
            GameId::Chess   => play_human_vs_computer(chess::ai, Chessboard::start())
        }
    }
}
