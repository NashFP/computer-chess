mod minimax;
mod interactive_play;
mod pennies;
mod fifteen;
mod othello;
mod chess;

use interactive_play::play_human_vs_computer;
use minimax::Game;
use minimax::best_move;
use pennies::Pennies;
use fifteen::Fifteen;
use othello::Othello;
use chess::Chessboard;

enum GameId { Pennies, Fifteen, Othello, Chess }

const WHICH : GameId = GameId::Chess;

fn main() {
    match WHICH {
        GameId::Pennies => play_human_vs_computer(best_move::<Pennies>, Pennies::start()),
        GameId::Fifteen => play_human_vs_computer(best_move::<Fifteen>, Fifteen::start()),
        GameId::Othello => play_human_vs_computer(othello::smarty_pants_ai, Othello::start()),
        GameId::Chess   => play_human_vs_computer(chess::ai, <Chessboard as Game>::start())
    }
}
