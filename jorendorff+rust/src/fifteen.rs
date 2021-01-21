// *** fifteen.rs: the game of Fifteen ****************************************

use std::fmt::Debug;
use std::fmt::Formatter;
use crate::minimax::Game;

#[derive(Clone, Copy)]
struct BitSet16(u16);

impl BitSet16 {
    fn has(&self, i: i32) -> bool {
        let BitSet16(bits) = *self;
        0 <= i && i < 16 && (1u16 << i) & bits != 0
    }

    fn with(&self, i: i32) -> BitSet16 {
        let BitSet16(bits) = *self;
        BitSet16(bits | (1u16 << i))
    }

    fn wins(&self) -> bool {
        // A bitset is a winner if it contains three distinct numbers
        // that add up to 15.
        (1 .. 8).any(|i| self.has(i) &&
            (i + 1 .. 9).any(|j| self.has(j) && {
                let k = 15 - (i + j);
                k > j && k <= 9 && self.has(k)
            }))
    }
}

impl Debug for BitSet16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        (0..16).filter(|i| self.has(*i)).collect::<Vec<i32>>().fmt(f)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Fifteen {
    red: BitSet16,
    blue: BitSet16
}

impl Game for Fifteen {
    type Move = i32;
    fn start() -> Fifteen {
        Fifteen { red: BitSet16(0), blue: BitSet16(0) }
    }
    fn moves(&self) -> Vec<i32> {
        if self.blue.wins() {
            Vec::new()
        } else {
            let Fifteen { red: BitSet16(r), blue: BitSet16(b) } = *self;
            let used = r | b;
            (1..10).filter(|i| (1u16 << i) & used == 0).collect::<Vec<i32>>()
        }
    }
    fn apply_move(&self, m: i32) -> Fifteen {
        Fifteen { red: self.blue, blue: self.red.with(m) }
    }
    fn score_finished_game(&self) -> f64 {
        if self.blue.wins() { 1.0 } else { 0.0 }
    }
}

