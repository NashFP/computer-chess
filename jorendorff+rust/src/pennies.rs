// *** pennies.rs: the game of Pennies ****************************************

use minimax::Game;

#[derive(Debug, Clone, Copy)]
pub struct Pennies(i32);

impl Game for Pennies {
    type Move = i32;

    fn start() -> Pennies {
        Pennies(14)
    }
    fn moves(&self) -> Vec<i32> {
        let Pennies(n) = *self;
        (1..4).filter(|x| n - x >= 0).collect::<Vec<i32>>()
    }
    fn apply_move(&self, m: i32) -> Pennies {
        let Pennies(n) = *self;
        Pennies(n - m)
    }
    fn score_finished_game(&self) -> f64 {
        1.0
    }
}
