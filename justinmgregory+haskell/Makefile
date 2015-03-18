default: play

.PHONY: play clean
play: chess-engine
	xboard -fcp ./chess-engine -debugMode true

chess-engine: Main.hs Board.hs AI.hs
	ghc --make Main -o chess-engine -O2

clean:
	-rm chess-engine
	-rm xboard-input.txt
	-rm *.hi
	-rm *.o
