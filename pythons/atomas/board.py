from random import choice

class Board:
    @staticmethod
    def is_pos(val):
        """Returns if the int is a possible move."""
        return val > 0 or val == -1

    @staticmethod
    def r_spawn():
        sel = list(range(1,4))*6
        return Board(*[choice(sel) for i in range(6)])
    
    class Node:
        """Static inner class... please ignore."""
        def __init__(self, data, ne = None, pr = None):
            self.data = data
            self.ne = ne
            self.pr = pr

    def __init__(self, *init):
        self.rear = None
        self.size = 0
        self.score = 0
        if len(init) > 0:
            for i in init:
                Board.__iadd__(self,i)

    def __iadd__(self, val):
        """self += v: Play v at the end.
           self += (i, v): Play v at the index i (indexing wraps)."""
        if type(val) == int and Board.is_pos(val):
            av = val
            tmp = self.rear
            app = True
        elif len(val) == 2 and Board.is_pos(val[1]):
            av = val[1]
            app = False
            if self.rear == None:
                tmp = self.rear
            else:
                if val[0] < 0:
                    idx = -1*val[0]
                    idx = self.size - idx % self.size
                else:
                    idx = val[0]
                tmp = self.rear
                for i in range(idx):
                    tmp = tmp.ne
        else:
            raise TypeError("Can only add integer or tuple of 2 ints!")

        self.size += 1

        if tmp == None:
            self.rear = Board.Node(av)
            self.rear.ne = self.rear.pr = self.rear
        else:
            tmp.ne = Board.Node(av, tmp.ne, tmp)
            tmp.ne.ne.pr = tmp.ne
            if app and tmp == self.rear:
                self.rear = self.rear.ne

            if av == -1:
                self._res_plus(tmp.ne)
            elif tmp.pr.data == -1:
                self._res_plus(tmp.pr)
            elif tmp.ne.ne.data == -1:
                self._res_plus(tmp.ne.ne)

        return self        

    def __iter__(self):
        """Loop through all the values."""
        i, tmp = 0, self.rear
        while i < len(self):
            i = i + 1
            tmp = tmp.ne
            yield tmp.data

    def __len__(self):
        """Get the length of the chain."""
        return self.size

    def __bool__(self):
        """Returns whether the game is not over."""
        return len(self) <= 16

    def __abs__(self):
        """Returns the score."""
        return self.score

    def _res_plus(self, node):
        """NEVER TOUCH THIS!"""
        l, r = node.pr, node.ne
        ct, rml = 0, False
        m, d_s = 0,0
        while l.data == r.data and l.data > 0 and l != r and l.ne != r:
            rml = rml or l == self.rear or r == self.rear
            ct += 2
            d_s += 2*l.data
            if l.data > m:
                m = l.data
            l, r = l.pr, r.ne

        if ct == 0:
            return
        elif l.ne == r:#new node only
            self.rear = Board.Node(m + ct//2)
            self.rear.ne = self.rear.pr = self.rear
            self.size = ct + 1
        else:#something survived
            l.ne = Board.Node(m + ct//2, r, l)
            r.pr = l.ne
            if rml:
                self.rear = l.ne

        self.size -= ct    
        self.score += d_s

        if r.data == -1:
            self._res_plus(r)
        if l.data == -1:
            self._res_plus(l)

    def __repr__(self):
        return str(list(self))

    def spawn(self):
        mi, mx = min(self), max(self)
        lb, rb = max(1, mi - 2), max(mx - 1, 3)
        spws = list([i]* (rb - i - lb) for i in range(lb, rb))
        if len(spws) >= 9:
            spws += [-1] * len(spws)//9
            spws += [-2] * len(spws)//19
        else:
            spws += [-1]    
        return choice(spws)

    def move(self, s = None):
        """Prompts for a move (prompting to be implemented by base classes)"""
        if not self:
            raise Exception("Cannot move in finished game!")
        
        if s == None:
            spawn = Board.spawn()
        else:
            spawn = s

        if spawn == -2:
            v, cp = self.do_minus()
            tmp = self.rear.ne
            for i in range(v):
                tmp = tmp.ne
            tmp.pr.ne = tmp.ne
            tmp.ne.pr = tmp.pr
            self.size -= 1
            if tmp.ne.data == -1:
                self._res_plus(tmp.ne)
            if tmp.pr.data == -1:
                self._res_plus(tmp.pr)
                
            if not cp:
                self += (tmp.data, self.do_move(tmp.data))
            else:
                self += (-1, self.do_move(-1))
        else:
            self += (spawn,self.do_move(spawn))

    def do_minus(self):
        raise NotImplemented("Implement this!")

    def do_move(self, spawn_of_satan):
        raise NotImplemented("Implement this!")
