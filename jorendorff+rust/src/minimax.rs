// *** minimax.rs: Generic minimax implementation *****************************


// === What is a game?

pub trait Game : Clone {
    type Move : Copy;

    fn start() -> Self;
    fn moves(&self) -> Vec<Self::Move>;
    fn apply_move(&self, Self::Move) -> Self;

    // Returns >0.0 if the last move won the game, 0.0 for a draw.
    fn score_finished_game(&self) -> f64;
}


// === How to play a game, if you're a computer

fn max<T, I>(mut iter: I) -> T
    where I: Iterator<Item=T>, T: PartialOrd
{
    let first = iter.next().expect("max: empty iterator");
    iter.fold(first, |a, b| if a > b { a } else { b })
}

fn max_by<T, I, F, M>(mut iter: I, score: F) -> T
  where
    I: Iterator<Item=T>,
    F: Fn(&T) -> M,
    M: PartialOrd
{
    let init_value = iter.next().expect("max_by: empty iterator");
    let init_score = score(&init_value);
    let (max_value, _) = iter.fold((init_value, init_score), |(v1, s1), v2| {
        let s2 = score(&v2);
        if s2 > s1 { (v2, s2) } else { (v1, s1) }
    });
    max_value
}

pub fn best_move<G: Game>(game: &G) -> G::Move {
    *max_by(game.moves().iter(), |m| score_move(game, **m))
}

fn score_move<G: Game>(game: &G, m: G::Move) -> f64 {
    score_game(&game.apply_move(m))
}

fn score_game<G: Game>(game: &G) -> f64 {
    let moves = game.moves();
    if moves.len() == 0 {
        game.score_finished_game()
    } else {
        -max(moves.iter().map(|m| score_move(game, *m)))
    }
}


// === How to play a game when you are in a hurry

pub fn best_move_with_depth_limit<F, G>(estimator: &F, move_limit: i32, g: &G) -> G::Move where
    F: Fn(&G) -> f64, G: Game
{
    let half_move_limit = move_limit * 2 - 1;
    let moves = g.moves();
    *max_by(moves.iter(), |m| score_move_with_depth_limit(estimator, half_move_limit, g, **m))
}

fn score_move_with_depth_limit<F, G>(estimator: &F, move_limit: i32, g: &G, m: G::Move) -> f64 where
    F: Fn(&G) -> f64, G: Game
{
    let g1 = g.apply_move(m);
    score_game_with_depth_limit(estimator, move_limit, &g1)
}

fn score_game_with_depth_limit<F, G>(estimator: &F, move_limit: i32, g: &G) -> f64 where
    F: Fn(&G) -> f64, G: Game
{
    let moves = g.moves();
    if moves.len() == 0 {
        g.score_finished_game()
    } else if move_limit == 0 {
        estimator(g)
    } else {
        -0.999 * max(moves.iter().map(|m| {
            score_move_with_depth_limit(estimator, move_limit - 1, g, *m)
        }))
    }
}
