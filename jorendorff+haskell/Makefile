play: Chess
	./Chess

playx: Chess
	xboard -fcp './Chess --xboard' -debugMode true

Chess: ChessMain.hs XBoard.hs Chess.hs Minimax.hs ChessAI.hs Vs.hs
	ghc -O ChessMain -o Chess

Reversi: Reversi.hs Minimax.hs Vs.hs
	ghc -O Reversi

Fifteen: Fifteen.hs Minimax.hs Vs.hs
	ghc -O Fifteen

Pennies: Pennies.hs Minimax.hs Vs.hs
	ghc -O Pennies

ChessTest: ChessTest.hs Chess.hs Minimax.hs ChessAI.hs XBoard.hs
	ghc -O ChessTest

all: Chess Reversi Fifteen Pennies ChessTest

check: ChessTest
	./ChessTest

.PHONY: play playx all check
