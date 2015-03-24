play: Chess
	./Chess +RTS -N4

playx: Chess
	xboard -fcp './Chess --xboard +RTS -N4' -debugMode true

GHCFLAGS=-O2 --make -threaded

Chess: ChessMain.hs XBoard.hs Chess.hs Minimax.hs ChessAI.hs Vs.hs
	ghc $(GHCFLAGS) ChessMain -o Chess

Reversi: Reversi.hs Minimax.hs Vs.hs
	ghc $(GHCFLAGS) Reversi

Fifteen: Fifteen.hs Minimax.hs Vs.hs
	ghc $(GHCFLAGS) Fifteen

Pennies: Pennies.hs Minimax.hs Vs.hs
	ghc $(GHCFLAGS) Pennies

ChessTest: ChessTest.hs Chess.hs Minimax.hs ChessAI.hs XBoard.hs
	ghc $(GHCFLAGS) ChessTest

all: Chess Reversi Fifteen Pennies ChessTest

check: ChessTest
	./ChessTest

clean:
	rm -f Chess Reversi Fifteen Pennies ChessTest *.hi *.o xboard.debug xboard-input.txt

.PHONY: play playx all clean check
