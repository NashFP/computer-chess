#!/usr/bin/env python3

""" compete.py - Play two programs against each other """

import sys, subprocess, re, threading, queue, time

class ChessPlayingProcess:
    def __init__(self, color, cmd):
        self.color = color

        # The sleep calls sprinkled throughout this program are a little hack
        # so that output from the two child processes usually isn't interleaved
        # line-by-line on your terminal. When that happens with two chessboards
        # it can be pretty hard to tell what's going on.
        time.sleep(0.5)
        print("*> starting process: {}".format(' '.join(cmd)))
        self.process = subprocess.Popen(cmd,
                                        bufsize=1, universal_newlines=True,
                                        stdin=subprocess.PIPE, stdout=subprocess.PIPE)

        self.q = queue.Queue()
        threading.Thread(target=self._reader).start()

    def _reader(self):
        for line in self.process.stdout:
            line = line.rstrip("\n")
            print(self.color + "> " + line)
            self.q.put(line)
        self.q.put(None)

    def parse_move(self, line):
        line = line.rstrip()
        if line == "" or line == ">" or line.startswith(("    ", "\t")):
            return None

        m = re.match(r'^(?:.*:)?\s*([a-h][1-8][a-h][1-8][qnrb]?|O-O|O-O-O|resign)$', line)
        if m is not None:
            should_close = False
            return m.group(1)
        print("*> WARNING: ignoring the line {!r} since it doesn't match".format(line))
        return None

    def _read_move(self):
        """ Read lines from q until one matches the pattern. Return the move. """

        time.sleep(0.5)

        while True:
            line = self.q.get()
            if line is None:
                break

            move = self.parse_move(line)
            if move is not None:
                return move

        # End of input without a move. Either the game is over, and our reply
        # doesn't matter; or it's a bug and we treat as resignation.
        return "resign"

    def send(self, cmd):
        time.sleep(0.5)
        print("*> sending {!r} to {}".format(cmd, self.color))
        self.process.stdin.write(cmd + "\n")

class MicrochessWhite(ChessPlayingProcess):
    def __init__(self):
        ChessPlayingProcess.__init__(self, 'W', ["jorendorff+cpp/microchess"])
        self.firstMove = True

    def play(self, move):
        if move == "resign":
            self.send("q")
        else:
            if self.firstMove:
                assert move is None
                self.firstMove = False
            else:
                # TODO - if this move is en passant or castling, we could paper over
                # Microchess's simplicity here; if promotion, we should probably
                # forfeit.
                self.send(move)
            self.send("p")  # hey microchess it is your turn

        return self._read_move()

class JorendorffHaskellBlack(ChessPlayingProcess):
    def __init__(self):
        ChessPlayingProcess.__init__(self, 'B', ["jorendorff+haskell/Chess"])

    def play(self, move):
        self.send(move)
        reply = self._read_move()
        if len(reply) == 4:
            # horrible kludge: haskell makes all its moves backwards,
            # so turn it around manually here (easier than doing the same thing in haskell)
            [c1, r1, c2, r2] = reply
            c1 = chr(ord('a') + ord('h') - ord(c1))
            r1 = chr(ord('1') + ord('8') - ord(r1))
            c2 = chr(ord('a') + ord('h') - ord(c2))
            r2 = chr(ord('1') + ord('8') - ord(r2))
            reply = c1 + r1 + c2 + r2
            print("*> reply translated to:", reply)
        return reply

    def parse_move(self, line):
        line = line.rstrip()
        line = re.sub(r"^your turn>.*$", ">", line)
        if line == "game over":
            line = "    game over"
        if line == "you win":
            line = "resign"
        return ChessPlayingProcess.parse_move(self, line)

def play(white, black):
    # Puzzle #1: Do you see anything wrong with this algorithm?
    # Puzzle #2: Why is it that way?
    blackMove = None
    while 1:
        whiteMove = white.play(blackMove)
        if blackMove == "resign":
            break
        blackMove = black.play(whiteMove)
        if whiteMove == "resign":
            break

play(MicrochessWhite(), JorendorffHaskellBlack())
