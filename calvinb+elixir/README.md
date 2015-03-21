# Chess in Elixir

A simple implementation of chess in Elixir. A work in progress. Just the basic moves so far. Does not yet support castling, en passant, or promotion. Also does not yet use any Erlangy process magic to aid performance, so it's not the speediest player.

## Installation

Assuming you have Erlang and Elixir installed, build using
```
mix escript.build
```

This builds an executable you can run directly.
```
./chess
```

Or to watch the game play itself, type
```
./chess auto
```

## How to play

Enter your move as a 4-character string in the format of [file][rank][file][rank], e.g., "a2a4" or "b1c3".

## The code

I'm still experimenting with module organization, and functions are frequently migrating from here to there. If you have ideas about how I might organize things better, I'm all ears.
