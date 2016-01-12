from board import Board
import tkinter as tk

class TkBoard(Board):
    def __init__(self, *init):
        self.v, self.c = -1, None
        self.s = None
        Board.__init__(self, *init)
    def move(self):
        Board.move(self, self.s)
    def do_move(self, sp):
        if self.v < 0:
            raise Exception()
        self.s = board.spawn()
        return self.v
    def do_minus(self):
        if self.v < 0 or self.c == None:
            raise Exception()
        self.s = board.spawn()
        return self.v, self.c

def render_board(board, can):
    pass

def get_ev(board, canvas):
    board.s = board.spawn()
    
    def click(event):
        pass

    def mp(event):
        pass
    
    canvas.bind("<Button-1>", click)
    canvas.bind("<Button-3>", mp)

def make_canvas(root):
    c = tk.Canvas(root)
    get_ev(TkBoard(*Board.r_spawn()), c)
    c.pack()
    return c

if __name__ == "__main__":
    r = tk.Tk()
    c = make_canvas(r)
    r.mainloop()
