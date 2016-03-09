from random import randint

class Mine:
    def __init__(self, v = 0, ll = None):
        self.v = v
        if ll is not None:
            self.ll = ll
        else:
            self.ll = []

    def is_open(self):
        return self.v < 0
    def open(self):
        if not self.is_open():
            self.v = -self.v - 1
            if self.is_mine():
                list(map(lambda x: x.loss_trigger(), self.ll))
        return self

    def is_flagged(self):
        return not self.is_open() and self.v >= 10
    def flag(self):
        if self.v < 0:
            raise Exception("Can't flag open mine!")
        elif self.v < 10:
            self.v += 10
        return self    
    def un_flag(self):
        if self.is_flagged():
            self.v = self.v - 10
        return self
    def toggle_flag(self):
        if self.is_flagged():
            self.un_flag()
        else:
            self.flag()
        return self

    def is_mine(self):
        if self.is_flagged():
            return self.v == 19
        elif self.is_open():
            return self.v == -10
        else:
            return self.v == 9

    def __add__(self, other):
        if type(other) != int:
            raise TypeError("Can only add ints to neighbour count!")
        elif not self.is_mine():
            return Mine(self.v + other, self.ll)
        else:
            return Mine(self.v, self.ll)

    def __str__(self):
        if self.is_flagged():
            return 'F'
        elif not self.is_open():
            return '#'
        elif self.is_mine():
            return '*'
        else:
            return str(abs(self))

    def __abs__(self):
        if self.is_open():
            return -(self.v + 1)
        elif self.is_flagged():
            return self.v - 10
        else:
            return self.v

    def add_loss_listener(self, v):
        self.ll.append(v)
        return self

class Board:
    @staticmethod
    def is_nbr(i1, i2, j1, j2):
        return abs(i1 - i2) <= 1 and abs(j1 - j2) <= 1 and (i1 != i2 or j1 != j2)

    @staticmethod
    def setup(h, w, mines):
        m = Board([[Mine() for i in range(w)] for j in range(h)])
        for i in range(h):
            for j in range(w):
                if mines >= 0 and randint(1,100) <= mines * 100 / (h * w)\
                    or h * w - i * w - j <= mines:
                    m.data[i][j] = Mine(9)
                    m = m.for_each_nbr(i, j, lambda x: x + 1)
                    mines = mines - 1
        return m

    def __init__(self, data):
        if type(data) == list and all(type(i) == list for i in data) and all(all(type(j) == Mine for j in i) for i in data):
            if(all(len(i)) == len(data[0]) for i in data):
                self.data = data
            else:
                raise ValueError("Only rectangular boards allowed!")
        else:
            raise TypeError("Can only have list of list of Mines!")

    def __abs__(self):
        return (len(self.data), len(self.data[0]))
    def __iter__(self):
        return iter(self.data)

    def __getitem__(self, val):#dumbly fliched off algebra/mat_utils.py
        if type(val) == int:
            return Board([[self.data[val]]])
        elif type(val) == slice:
            return Board([self.data[val]])
        elif type(val) == tuple and len(val) == 2 and type(val[0]) in [int,slice] and type(val[1]) in [int,slice]:
            if type(val[0]) == slice:
                return Board(list(map(lambda x: x[val[1]] if type(val[1]) == slice else [x[val[1]]], self.data[val[0]])))
            elif type(val[1]) == slice:
                return Board(self.data[val[0]][val[1]])
            else:
                return self.data[val[0]][val[1]]
        else:
            raise TypeError("Invalid index.")

    def for_each(mat, fun, ret_board = True):
        v = [[fun(i, j, mat[(i,j)]) for j in range(abs(mat)[1])] for i in range(abs(mat)[0])]
        if ret_board:
            return Board(v)
        else:
            return v

    def for_each_nbr(mat, i, j, fun, pa = False):
        def sp(r, c, v):
            if Board.is_nbr(i, r, j, c):
                if pa:
                    return fun(r, c, v)
                else:
                    return fun(v)
            else:
                return v
        return mat.for_each(sp)    

    def __str__(mat):
        s = '\n'.join(' ' + chr(ord('a') + i) + ' |' + ''.join(' ' + str(x) + ' ' for x in mat.data[i]) for i in range(abs(mat)[0]))
        s += '\n' + ('   +' + 3 *  abs(mat)[1] * '-')
        s += '\n' + ('    ' + ''.join(' ' + chr(i) + ' ' for i in range(ord('a'), ord('a') + abs(mat)[1])))
        return s

    def __bool__(self):
        v = self.for_each(lambda r, c, v: v.is_open() or v.is_flagged(), False)
        return all(map(all,v))

