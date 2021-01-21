// *** interactive_play.rs: Command-line play vs. a computer opponent *********

use std::fmt::Debug;
use std;
use std::io::Write;
use std::io::stdin;
use std::io::stdout;
use std::str::FromStr;
use minimax::Game;

fn input_one_of<T>(options: &[T], prompt: &str) -> std::io::Result<Option<T>> where
    T : Copy + Debug + FromStr + PartialEq
{
    loop {
        stdout().write(prompt.as_bytes())?;
        stdout().flush()?;
        let mut line_buf = String::new();
        stdin().read_line(&mut line_buf)?;
        if line_buf.len() == 0 {
            return Ok(None);  // user typed the EOF key, treat it like "q"
        }
        let line_str : &str = line_buf.as_ref();
        let line = line_str.trim();

        if line == "q" {
            return Ok(None);
        }
        if line == "?" {
            println!("options: {:?}", options);
            continue;
        }
        match T::from_str(&line) {
            Err(_) => {
                println!("i didn't understand that");
                continue;
            }
            Ok(m) => {
                if options.contains(&m) {
                    return Ok(Some(m));
                } else {
                    println!("{:?} is not an option (enter ? to show all options)", m);
                }
            }
        }
    }
}

pub fn play_human_vs_computer<G: Game + Debug, F>(select_move: F, start: G)
  where
    G::Move: PartialEq + Debug + FromStr,
    F: Fn(&G) -> G::Move
{
    println!("{:?}", start);

    let mut board = start;

    loop {
        let options = board.moves();
        if options.len() == 0 {
            println!("game over");
            let s = board.score_finished_game();
            println!("{}",
                if s == 0.0 {
                    "it's a tie"
                } else if s > 0.0 {
                    "i win"
                } else {
                    "you win"
                });
            return;
        }
        let your_move = match input_one_of(&options, "your turn> ").unwrap() {
            None => {
                return; // user typed "q" to quit
            },
            Some(m) => m
        };

        board = board.apply_move(your_move);
        println!("{:?}", board);
        let move_vec = board.moves();
        if move_vec.len() == 0 {
            println!("game over");
            let s = board.score_finished_game();
            println!("{}",
                if s == 0.0 {
                    "it's a tie"
                } else if s > 0.0 {
                    "you win"
                } else {
                    "i win"
                });
            return;
        }

        // computer's turn
        let my_move = select_move(&board);
        println!("my move: {:?}", my_move);
        board = board.apply_move(my_move);
        println!("{:?}", board);
    }
}
