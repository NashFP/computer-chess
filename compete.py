#!/usr/bin/env python3

""" compete.py - Play two programs against each other """

import sys, subprocess, io, re

def read_until_prompt(color, f, prompt_chars):
    """
    Read bytes from f until we get a partial line that ends with prompt_chars.
    Return all the text we read, converted to characters using UTF-8.
    Also echo all the text to stdout.

    This function may be fooled if the input contains prompt_chars at the beginning of a line
    and it's not meant as a prompt.  That's life!
    """
    prompt_bytes = prompt_chars.encode('utf-8')
    text = ""
    partial = b''
    while True:
        chunk = f.read(16*1024)
        if len(chunk) == 0:
            # end of output! may still have partial-line data though, which is weird
            break

        last_newline = chunk.rfind(b'\n')
        if last_newline == -1:
            partial += chunk
        else:
            cut = last_newline + 1
            chunk, partial = partial + chunk[:cut], chunk[cut:]
            for line in io.TextIOWrapper(io.BytesIO(chunk)):
                assert line.endswith("\n")
                print(color + "> " + line.rstrip("\n"))
                text += line

        if partial == prompt_bytes:
            # The chunk we just read ends with what looks like a prompt.  It's
            # possible we just have really bad luck and more output is coming.
            # But let's assume it's a prompt.
            break

    if partial:
        partial_chars = partial.decode('utf-8')
        print(color + "> " + partial_chars)
        text += partial_chars
    return text

class ChessPlayingProcess:
    def __init__(self, color, cmd, prompt):
        self.color = color
        self.prompt = prompt
        self.closed = False

        print("*> starting process: {}".format(' '.join(cmd)))
        self.process = subprocess.Popen(cmd,
                                        bufsize=0, stdin=subprocess.PIPE, stdout=subprocess.PIPE)

    def shutdown(self):
        if not self.closed:
            self.send("q")
            self.ignore_until_prompt()
            if not self.closed:
                print("*> player {} sent another prompt after quitting! killing it".format(self.color))
                self.closed = True
                self.process.terminate()
                return
        self.process.wait()
        print("*> player {} exited".format(self.color))

    def parse_move(self, line):
        line = line.rstrip()
        if line == "" or line.endswith(">") or line.startswith(("    ", "\t")):
            return None

        m = re.match(r'^(?:.*:)?\s*([a-h][1-8][a-h][1-8][qnrb]?|O-O|O-O-O|resign)$', line)
        if m is not None:
            should_close = False
            return m.group(1)
        print("*> ignoring the line {!r} since it doesn't match".format(line))
        return None

    def _readlines_until_prompt(self):
        text = read_until_prompt(self.color, self.process.stdout, self.prompt)
        self.closed = not text.endswith(self.prompt)
        return text.splitlines()

    def ignore_until_prompt(self):
        for line in self._readlines_until_prompt():
            line_as_move = self.parse_move(line)
            if line_as_move is not None:
                print("*> ignoring what looks like a move ({}) sent by player {}".format(
                    line_as_move, self.color))

    def read_move(self):
        """ Read lines from q until one matches the pattern. Return the move. """

        if self.closed:
            print("*> trying to read from closed pipe, assuming resignation")
            return "resign"

        move = None
        for line in self._readlines_until_prompt():
            line_as_move = self.parse_move(line)
            if line_as_move is not None:
                if move is None:
                    move = line_as_move
                else:
                    print("*> found another move in the output, uh oh. ignoring it")

        return move

    def send(self, cmd):
        print("*> sending {!r} to {}".format(cmd, self.color))
        self.process.stdin.write((cmd + "\n").encode('ascii'))

class MicrochessWhite(ChessPlayingProcess):
    def __init__(self):
        ChessPlayingProcess.__init__(self, 'W', ["jorendorff+cpp/microchess"], '> ')
        read_until_prompt(self.color, self.process.stdout, self.prompt)
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
                self.ignore_until_prompt()
            self.send("p")  # hey microchess it is your turn

        reply = self.read_move()
        if reply == "resign":
            self.send("q")  # the program doesn't automatically exit when it resigns
            self.ignore_until_prompt()
        return reply

class JorendorffHaskellBlack(ChessPlayingProcess):
    def __init__(self):
        ChessPlayingProcess.__init__(self, 'B', ["jorendorff+haskell/Chess"], 'your turn> ')
        read_until_prompt(self.color, self.process.stdout, self.prompt)

    def play(self, move):
        if move == "resign":
            move = "q"
        self.send(move)
        return self.read_move()

    def parse_move(self, line):
        line = line.rstrip()
        if line == "game over":
            line = "    game over"
        if line == "you win":
            line = "resign"
        return ChessPlayingProcess.parse_move(self, line)

def play(white, black):
    # Puzzle #1: Can you spot the weirdness in this algorithm?
    # Puzzle #2: Why is it that way?
    blackMove = None
    while 1:
        whiteMove = white.play(blackMove)
        if blackMove == "resign":
            break
        blackMove = black.play(whiteMove)
        if whiteMove == "resign":
            break
    white.shutdown()
    black.shutdown()

play(MicrochessWhite(), JorendorffHaskellBlack())
