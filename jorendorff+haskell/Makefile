play: Chess
	./Chess

Chess: ChessMain.hs XBoard.hs Chess.hs Minimax.hs ChessAI.hs Vs.hs
	ghc -O ChessMain -o Chess

Reversi: Reversi.hs Minimax.hs Vs.hs
	ghc -O Reversi

playx: Chess
	xboard -fcp './Chess --xboard' -debugMode true

ChessTest: ChessTest.hs Chess.hs Minimax.hs ChessAI.hs XBoard.hs
	ghc -O ChessTest

check: ChessTest
	./ChessTest

.PHONY: play playx check
