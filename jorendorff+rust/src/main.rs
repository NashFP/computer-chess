mod minimax;
mod interactive_play;
mod pennies;
mod fifteen;
mod othello;

use interactive_play::play_human_vs_computer;
use minimax::Game;
use minimax::best_move;
use pennies::Pennies;
use fifteen::Fifteen;
use othello::Othello;

enum GameId { Pennies, Fifteen, Othello }

const WHICH : GameId = GameId::Othello;

fn main() {
    match WHICH {
        GameId::Pennies => play_human_vs_computer(best_move::<Pennies>, Pennies::start()),
        GameId::Fifteen => play_human_vs_computer(best_move::<Fifteen>, Fifteen::start()),
        GameId::Othello => play_human_vs_computer(othello::smarty_pants_ai, Othello::start())
    }
}
