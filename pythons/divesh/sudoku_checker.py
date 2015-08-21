# checks if a 2d array is a solution to a sudoku puzzle
# currently runs py3, but only in test_checker...
from functools import reduce

def iter_cols(mat):
  """ Loops through the cols of a matrix.
      Matrix cannot be ragged (must be rectangular).
      Yields the column as a list."""
  for i in range(len(mat[0])):
    yield [mat[j][i] for j in range(len(mat))]


def check_sol(sol):
  """  Checks if a 2d array is a solution to a sudoku puzzle.
       Returns a boolean."""
  
  #checks if all the elements are ints between 1 and 9
  if not all(all(1<= i <= 9 and i == int(i) for i in j) for j in sol):
    return False

  #checks each row for repeats. (If there's a repeat, the set would be shorter than the list.)
  if not all(len(set(i)) == len(i) for i in sol):
    return False

  #checks each column.
  if not all(len(set(i)) == len(i) for i in iter_cols(sol)):
    return False

  #check every square.
  if not all(len(set(range(1,10)).difference(set(reduce(list.__add__,(i[j%3*3:j%3*3+3] for i in sol[j//3:j//3+3]))))) == 0 for j in range(9)):
    return False

  #everything was fine!
  return True


"""
Explanation (by if statement):

1) This one checks whether the elements are integers (or whole floats) and between 1 and 9.
   The expression with the "for i in j for j in sol" is a list comprehension that loops through every
   item in sol. The expression to its left (evaluated last) is then run on i. It evaluates to a boolean.
   "all" takes in the iterable and makes sure every element is true.

2) This checks each row for repeats. If there is a repeat, set would have discarded it (that is the
   definition of a set) and therefore, the set would be shorter.

3) This checks each column in the exact same way.

4) This is the hard one: it checks every square. There are two sets made in this. The first set has all the
   elements of the square and the second has the integers from 1 to 9. If there is no difference in the sets,
   then the set difference would be the empty set, as tested. The set of elements is made in a seemingly complex way.
   The idea is to see the indices of each square (within the bigger matrix) as an odd slice: a slice of a slice. These
   are:

   0:2    0:2    0:2  (The right indices are included in each slice.) The slice with the mod operator (performed on rows
   0:2    3:5    6:8  to generate the columns) has the indices on the bottom. The other slice is given by the top.
                      The "set(range(1,10))" gives a set of all integers 1 - 9 and the difference checks if this set has
   3:5    3:5    3:5  anything the other set does not (at which point the square has missed a number).
   0:2    3:5    6:8
                      reduce is a higher order function that 'flattens' the innermost list comprehension (this innermost produces
   6:8    6:8    6:8  a 3x3 matrix otherwise). reduce works as follows: imagine you have a function f as:
   0:2    3:5    6:8  >>> def f(a,b):pass #but it instead returns something

   Then, let's say you have:
   >>> lst = [a,b,c] #some random values.
   >>> reduce(f,lst)
   would be the same as
   >>> f(f(a,b),c)
   
   This whole mess is passed to all and all makes sure every square meets the conditions. Ta-da!
"""

def test_checker(times = 20):
  from random import shuffle
  lst = list(range(1,10))
  for i in range(times):
    mat = []
    for i in range(9):
      shuffle(lst)
      mat.append(lst)
      print(lst)
    print("Is this a valid one? " + str(check_sol(mat)))
    input("Enter to move on > ")

if __name__ == "__main__":
  test_checker()
