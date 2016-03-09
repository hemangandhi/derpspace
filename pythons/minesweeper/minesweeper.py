#! /usr/bin/env python3

from util import *

def validate_prpt(prompt, validator, converter = lambda x: x):
    s = input(prompt)
    while not validator(s):
        s = input(prompt)
    return converter(s)    

class Game:
    @staticmethod
    def run():
        h = validate_prpt("Enter the board height: ", str.isdigit, int)
        w = validate_prpt("Enter the board width: ", str.isdigit, int)
        m = validate_prpt("Enter the board mine count: ", str.isdigit, int)
        g = Game(h, w, m)
        while g:
            print(g)
            validate_prpt("Enter a char (f to flag) and the letters on the side: ", g.validate_input, g.parse_input)
        print(g)
        print(g.open_all())    

    def __init__(self, h, w, m):
        self.board = Board.setup(h, w, m).for_each(lambda r, c, v: v.add_loss_listener(self))
        self.state = -1

    def loss_trigger(self):
        self.state = 1

    def __str__(self):
        if self.state == -1:
            s = "Playing...\n"
        elif self.state == 0:
            s = "You won!\n"
        else:
            s = "You lost!\n"
        return s + str(self.board)    

    def __bool__(self):
        return self.state < 0

    def validate_input(self, inp):
        if len(inp) != 3:
            return False
        elif inp[1] < 'a' or inp[1] >= chr(ord('a') + abs(self.board)[0]):
            return False
        elif inp[1] < 'a' or inp[1] >= chr(ord('a') + abs(self.board)[1]):
            return False
        else:
            return True

    def parse_input(self, inp):
        if inp[0] == 'f':
            self.board[(ord(inp[1]) - ord('a'), ord(inp[2]) - ord('a'))].flag()
        else:
            i = ord(inp[1]) - ord('a')
            j = ord(inp[2]) - ord('a')
            def op_all(r, c, v):
                v.open()
                if abs(v) == 0:
                    self.board.for_each_nbr(r, c, lambda r,c,v: op_all(r,c,v) if not v.is_open() else v, True)
                return v
            op_all(i, j, self.board[(i, j)])
        
        if self.state == -1 and self.board:
            self.state = 0
    
    def open_all(self):
        return "The board was:\n" + str(self.board.for_each(lambda r, c, v: v.open() if not v.is_flagged() else v))

if __name__ == "__main__":
    Game.run()
