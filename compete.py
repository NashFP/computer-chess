#!/usr/bin/env python3

""" compete.py - Play two programs against each other """

import sys, subprocess, re, threading, queue, time

def make_program_output_queue(color, f):
    q = queue.Queue()
    def reader():
        for line in f:
            line = line.rstrip("\n")
            print(color + "> " + line)
            q.put(line)
        q.put(None)
    threading.Thread(target=reader).start()
    return q

def common(q):
    """ Read lines from q until one matches the pattern. Return the move. """

    time.sleep(0.5)
    should_close = True

    while True:
        line = q.get()
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


def microchess_as_white():
    time.sleep(0.5)
    white = subprocess.Popen(["jorendorff+cpp/microchess"],
                             bufsize=1, universal_newlines=True,
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    q = make_program_output_queue('W', white.stdout)
    firstMove = [True]

    def play(move):
        if firstMove[0]:
            assert move is None
            firstMove[0] = False
        else:
            # TODO - if this move is en passant or castling, we could paper over
            # Microchess's simplicity here; if promotion, we should probably
            # forfeit.
            time.sleep(0.5)
            white.stdin.write(move + "\n")

        time.sleep(0.5)
        white.stdin.write("p\n")  # hey microchess it is your turn
        return common(q)

    return play

def jorendorff_haskell_as_black():
    time.sleep(0.5)
    black = subprocess.Popen(["jorendorff+haskell/Chess"],
                             bufsize=1, universal_newlines=True,
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    q = make_program_output_queue('B', black.stdout)

    def play(move):
        time.sleep(0.5)
        black.stdin.write(move + "\n")
        reply = common(q)
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

    return play

def play(white, black):
    lastMove = None
    while 1:
        lastMove = white(lastMove)
        if lastMove == "resign":
            break
        lastMove = black(lastMove)
        if lastMove == "resign":
            break


play(microchess_as_white(), jorendorff_haskell_as_black())
