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

    def _read_move(self):
        """ Read lines from q until one matches the pattern. Return the move. """

        time.sleep(0.5)
        should_close = True

        while True:
            line = self.q.get()
            if line is None:
                break

            line = line.rstrip()
            if line == "" or line == ">" or line.startswith(("    ", "\t")):
                continue

            m = re.match(r'^(?:.*:)?\s*([a-h][1-8][a-h][1-8]|O-O|O-O-O|resign)$', line)
            if m is not None:
                should_close = False
                return m.group(1)
            print("*> WARNING: ignoring the line {!r} since it doesn't match".format(line))

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

def play(white, black):
    lastMove = None
    while 1:
        lastMove = white.play(lastMove)
        if lastMove == "resign":
            break
        lastMove = black.play(lastMove)
        if lastMove == "resign":
            break

play(MicrochessWhite(), JorendorffHaskellBlack())
