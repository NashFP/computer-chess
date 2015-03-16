run: microchess
	./microchess

microchess: microchess.cpp
	g++ -o microchess microchess.cpp

check: microchess
	python tests/test.py

.PHONY: run check
