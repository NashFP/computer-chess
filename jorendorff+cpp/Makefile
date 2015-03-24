play: microchess
	./microchess

playx: microchess
	xboard -mode MachineWhite -fcp './microchess --xboard' -debugMode true

playx-vs: microchess
	$(MAKE) -C ../jorendorff+haskell Chess
	xboard -mode TwoMachines -fcp './microchess --xboard' -scp '../jorendorff+haskell/Chess --xboard' -debugMode true

microchess: microchess.cpp
	g++ -o microchess microchess.cpp

check: microchess
	python tests/test.py

.PHONY: play playx check
