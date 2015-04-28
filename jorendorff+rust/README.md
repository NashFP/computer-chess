# Chess-playing program in Rust

This is a straight-up port of the Haskell program in [../jorendorff+haskell]().
It's functionally identical;
the two programs make the same moves, given the same circumstances.

Rust is not really a functional programming language.
I don't know what it is, except fast.
It's definitely fast.
The untuned, single-threaded Rust program,
using the naive minimax algorithm
is faster than the slightly-tuned Haskell program,
which implements alpha-beta pruning
and runs on 4 cores.

The next steps for this program would be

*   implement alpha-beta
*   spread the work across multiple cores (should be easy in Rust)
*   add a transposition table (a little easier in Rust than in Haskell)
*   profile it
