#! /usr/bin/env python

from random import choice

"""
HOW TO USE:
** This file is all you need to modify to create bots for the six game.
   To have a bot registered to the server here, just do as follows:

1) Have a function that accepts 3 parameters: the game state matrix, the possible
   moves and the turn. (Each will be explained.) Ensure that it returns a value from
   the possible moves.
2) Add it to the dictionary, bots_dict, by providing the key as a name (for users) and
   the value as an address. Use @{function's name} for a default address to be used.
   The provided address will be given an HTTP POST with the three parameters as JSON,
   see the server for details. If you provide your own address, you're responsible for
   that server.

THE PARAMETERS PASSED TO A BOT:
** The parameters are (in order): the game matrix, the possible moves, and the current player.

** The current player is a boolean that is true if the bot is playing for red.

** The game matrix is a list of three-value lists. The inner lists have the format (x, y, is_red) (where
   is_red is true if the piece at (x, y) is red).

** The matrix of possible moves is a list of (x, y) points where moves can be made.

THE COORDINATE SYSTEM USED:
To represent hexagons, the following system is used (relative to a hexagon at (x, y)):

            (x,y + 1) == positive y
             _______
            /       \
 (x-1,y+1) /         \  (x + 1, y) == positive x
          /           \
          \  (x, y)   /
  (x-1,y)  \         /  (x + 1, y - 1)
  (neg. x)  \       /
             -------
            (x, y - 1)
            (neg. y)

The global variable (tuple of tuples) "dels" is provided to give the neighbors about
(0, 0) from the top right in clockwise order.
"""

bots_dict = {'random mover': '@rand_move_bot',
             'maximal adjacency': '@max_adj_bot',
             'smarter max adjacency': '@smarter_adj'}

dels = ((1,0),(1,-1),(0,-1),(-1,0),(-1,1),(0,1))

def rand_move_bot(mat, pos, trn):
  """A bots that selects a random possible move."""
  return choice(pos)

def n_adj(mat, trn, x, y):
  """The number of same-color adjacent cells in the matrix about (x, y)."""
  c = 0
  for i in dels:
    if [i[0] + x, i[1] + y, trn] in mat:
      c = c + 1
  return c

def max_adj_bot(mat, pos, trn):
  """A bot that picks the move with the
     most adjacent pieces of the same color."""
  mpi = 0
  mp = n_adj(mat, trn, *pos[0])
  for i in range(1, len(pos)):
    c = n_adj(mat, trn, *pos[i])
    if c > mp:
      mp = c
      mpi = i
  return pos[mpi]

def is_win(mat, x, y, is_red):
  """Checks if a placement of is_red at (x, y) would be a win.
     Assumes that [x, y, is_red] is not in mat."""
  line_lens = []
  for i in dels:
    j = 0
    while [i[0]*(j+1) + x, i[1]*(j+1) + y, is_red] in mat:
      j = j + 1
    line_lens.append(j)

  if any(line_lens[i] + line_lens[i+3] >= 5 for i in range(3)):
    return True

  for j in range(6):
    k = (j + 1) % 6
    if line_lens[k] >= 2 and line_lens[j] >= 2 and [x + dels[k][0] + dels[j][0], y + dels[k][1] + dels[j][1], is_red] in mat:
      return True

  for k in range(6):
    if line_lens[k] >= 1:
      xp = x + dels[k][0]
      yp = y + dels[k][1]
      for j in range(1,6):
        if [xp, yp, is_red] in mat:
          xp = xp + dels[(k + j) % 6][0]
          yp = yp + dels[(k + j) % 6][1]
        else:
          break
      else:
        return True

  for k in range(6):
    if all(line_lens[(k + j) % 6] >= 1 for j in range(4)):
      if [x + dels[(k + 1) % 6][0] + dels[(k + 2) % 6][0], y + dels[(k + 1) % 6][1] + dels[(k + 2) % 6][1], is_red] in mat:
        return True

  return False

def smarter_bot(bot):
  """Takes a bot and returns a bot that will:
     1) Play immediate wins.
     2) Block immediate losses.
     3) Otherwise, behave like the original bot.

     This is also a decorator, so can be used as:
     @smarter_bot
     def dumb_bot(mat, pos, trn): #etc..."""
  def smart(mat, pos, trn):
    w = list(filter(lambda x: is_win(mat, x[0], x[1], trn), pos))
    fil = list(filter(lambda x: is_win(mat, x[0], x[1], not trn), pos))
    if len(w) == 1:
      return w[0]
    elif len(w) > 1:
      return bot(mat, w, trn)
    elif len(fil) == 0:
      return bot(mat, pos, trn)
    elif len(fil) > 1:
      return bot(mat, fil, trn)
    else:
      return fil[0]
  return smart

smarter_adj = smarter_bot(max_adj_bot)
