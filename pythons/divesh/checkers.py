#! /usr/bin/env python3
def mid_point(p1, p2):
  return ((p1[0] + p2[1])/2,(p1[1] + p2[1])/2)


class Player:
  def __init__(self, is_white, name):
    self.name = name
    self.white = is_white
    if is_white:
      self.ctrs = {(i, 2*j + i % 2, False) for i in range(3) for j in range(4)}
    else:
      self.ctrs = {(i, 2*j + i % 2, False) for i in range(7,4,-1) for j in range(4)}

  def is_king(self, x, y):
    return (x, y, True) in self.ctrs

  def get_ctr(self, x, y):
    if (x, y) in self:
      return (x, y, self.is_king(x, y))
    else:
      return None

  def __len__(self):
    return len(self.ctrs)

  def __isub__(self, other):
    if type(other) == set:
      for i in other:
        if len(i) == 2:
          self.ctrs -= {(*i, self.is_king(i))}
        elif len(i) == 3:
          self.ctrs -= {i}
    else:
      raise TypeError("Can only subtract a set of counters!")

  def move(self):
    raise NotImplementedError("Move is not implemented for abstract player!")

  def has_lost(self):
    return len(self) == 0

  def __repr__(self):
    return name + " with " + len(self) + " counters."

  def __contains__(self, val):
    if len(val) == 3:
      return val in self.ctrs
    elif self.is_king(*val):
      return True
    else:
      return (*val, False) in self.ctrs

  def __iter__(self):
    return iter(self.ctrs)

  def __sub__(self, other):
    o = set(get_ctr(i[0], i[1]) for i in other)
    ns = self.ctrs - o
    p = type(self)(self.white, self.name + "v")
    p.ctrs = ns
    return p

  def __add__(self, other):
    if type(other) != set or not all(len(i) == 3 for i in other):
      raise TypeError("Cannot add with this type!")
    t = self.ctrs | other
    p = type(self)(self.white, self.name + "V")
    p.ctrs = t
    return p

  def __iadd__(self, other):
    self = self + other
    self.name = self.name[:len(self.name) - 1]

  def moved(self, old, new):
    ik = self.is_king(old)
    return self + (*new, ik) - old

  def do_move(self, old, new):
    self = self.moved(old, new)
    self.name = self.name[:len(self.name) - 1]

  def __setitem__(self, val, new):
    self.move(val, new)

class Board:
  def __init__(self, names):
    if len(names) != 2:
      raise ValueError("Incorrect number of players.")
    self.white = Player(True, names['white'])
    self.black = Player(False, names['black'])

  def __contains__(self, pt):
    return 0 <= pt[0] < 8 and 0 <= pt[1] < 8

  def is_empty(self, x, y):
    return (x, y) not in self.black and (x, y) not in self.white

  def is_black(self, x, y):
    return (x, y) in self.black

  def is_white(self, x, y):
    return (x, y) in self.white

  def is_color(self, x, y, white):
    if white:
      return self.is_white(x, y)
    else:
      return self.is_black(x, y)

  def check_simple_move(self, src, dest, white):
    if self.is_empty(*src) or src not in self:
      return False
    if not self.is_empty(*dest):
      return False
    elif dest not in self:
      return False

    src = self.white.get_ctr(src[0], src[1]) if white else self.black.get_ctr(src[0], src[1])

    d_y = dest[1] - src[1]
    d_x = dest[0] - src[0]
    if abs(d_x) != abs(d_y):
      return False
    elif int(white or (src[2] == True and not white)) == d_y or int(not white or (src[2] == True and white)) == -1*d_y:
      return True
    elif int(white or (src[2] == True and not white)) == d_y/2 or int(not white or (src[2] == True and white)) == -1*d_y/2:
      return self.is_color(src[0] + d_x/2, src[1] + d_y/2, not white)
    else:
      return False

  def check_move_list(self, white, *moves):
    if len(moves) > 2 and all(abs(moves[i][0] - moves[i - 1][0]) == 2 for i in range(1, len(moves))):
      temp = self.white if white else self.black
      ot = self.black if white else self.white
      for i in range(1, len(moves)):
        if self.check_simple_move(moves[i - 1], moves[i], white):
          temp = (temp + {(*moves[i],temp.is_king(*moves[i - 1]))}) - {moves[i]}
          ot = ot - {mid_point(moves[i-1], moves[i])}
        else:
          return -1
      return ot
    elif len(moves) == 2:
      if check_simple_move(moves[0], moves[1], white):
        return self.black - {mid_point(*moves)} if white else self.white - {mid_point(*moves)}
    else:
      return -1

  def virtual_move(self, white, *moves):
    s = self.check_move_list(white, *moves)
    if type(s) != int:
      board_t = type(self)({'white': self.white.name, 'black': self.black.name})
      if white:
        board_t.white.do_move(moves[0], moves[len(moves) - 1])
        boart_t.black = s
      else:
        board_t.black.do_move(moves[0], moves[len(moves) - 1])
        board_t.white = s
    return board_t

  def do_move(self, white, *moves):
    self = self.virtual_move(white, *moves)

  def __bool__(self):
    return not (self.white.has_lost() or self.black.has_lost())

class SmarterBoard(Board):
  pass
