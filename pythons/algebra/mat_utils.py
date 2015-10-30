#A Matrix utility class in py 3.x

class Matrix:
    """
    A simple Matrix class for representing mathematical matrices.
    """
    
    def validate(m):
        """
        STATIC METHOD!
        Makes sure that m is a list of lists of floats or ints.
        Returns True for matrices.
        """
        if type(m) == Matrix:
            return True
        v = type(m) == list and all(type(i) == list for i in m)
        v = v and all(type(j) in [float,int] for i in m for j in i)
        v = v and all(len(i) == len(m[0]) for i in m[1:])
        return v

    def identity(dim):
        """STATIC METHOD: Returns the identity matrix of size dim."""
        return Matrix([[0 for i in range(j)] + [1] + [0 for i in range(dim - j - 1)] for j in range(dim)])
        
    def __init__(self, data):
        """
        Make a matrix with the data passed in.
        Instances should not be mutated in code as this may break
        this implementation.

        Instances are more or less lists of lists. In place changing cannot
        be done directly and isn't recommended. The +,* and - operator work
        as ordained by math.
        """
        if not Matrix.validate(data):
            raise TypeError("The matrix passed in must be a rectangular list of list of int or float")
        elif type(data) != Matrix:
            self.data = data
        else:
            self.data = data.copy().data

    def copy(self):
        """Returns a copy of the current matrix."""
        return Matrix([[i for i in j] for j in self])

    def get_col(self,ind):
        """Generator iterating over the specified column of self."""
        yield from map(lambda x: x[ind], self)

    def dim(self):
        """Returns [rows,columns]"""
        return [len(self),len(self[0])]

    def __iter__(self):
        """Returns an iterator over the rows of the matrix."""
        return iter(self.data)

    def __len__(self):
        """Returns the number of rows."""
        return len(self.data)

    def __getitem__(self, val):
        """
        If an int is passed in, returns that row,
        if a tuple of 2 ints is passed in, returns that
        value (assuming the tuple is (row,col)).

        If the tuple has a slice, the first slice is applied to rows
        and the second slice (or indexing) is applied to columns.

        A Matrix is returned when a single slice is passed in in any way.
        """
        if type(val) == int:
            return self.data[val]
        elif type(val) == slice:
            return Matrix(self.data[val])
        elif type(val) == tuple and len(val) == 2 and type(val[0]) in [int,slice] and type(val[1]) in [int,slice]:
            if type(val[0]) == slice:
                return Matrix(list(map(lambda x: x[val[1]] if type(val[1]) == slice else [x[val[1]]], self.data[val[0]])))
            elif type(val[1]) == slice:
                return Matrix([self.data[val[0]][val[1]]])
            else:
                return self.data[val[0]][val[1]]
        else:
            raise TypeError("Invalid index.")

    def __eq__(self, other):
        """Checks if 2 matrices are equal. Can compare to lists of lists as well."""
        v = Matrix.validate(other) and len(other) == len(self) and len(other[0]) == len(self[0])
        v = v and all(other[i][j] == self[i][j] for i in range(len(self)) for j in range(len(self[i])))
        return v

    def __add__(self, other):
        """Adds 2 matrices. Can add with list of lists."""
        if not Matrix.validate(other) or len(self) != len(other) or len(self[0]) != len(other[0]):
            raise ValueError("Matrix dimensions must match")
        return Matrix([[self[i][j] + other[i][j] for j in range(len(self[i]))] for i in range(len(self))])

    def __mul__(self, other):
        """Multiplies matrices to each other, lists of lists or scalars, depending on the argument."""
        if type(other) in [int,float]:
            return Matrix([[i*other for i in j] for j in self])
        elif Matrix.validate(other) and len(self[0]) == len(other):
            return Matrix([[sum(self[j][k] * other[k][i] for k in range(len(self[0]))) for j in range(len(other[0]))] for i in range(len(self))])
        else:
            raise TypeError("Can only multiply with int, float, Matrix or 2D list of int or float.")

    def __rmul__(self, other):
        try:
            return Matrix(other) * self
        except TypeError:
            return self * other

    def __sub__(self, other):
        """Subtracts a matrix from another or a list of lists."""
        return self + (-1*Matrix(other))

    def __repr__(self):
        """Gives a neat string with each row in its own line."""
        r = ""
        for i in self:
            r += str(i) + "\n"
        return r    

    def transpose(self):
        """Returns a matrix with the rows as columns of this one."""
        return Matrix([list(self.get_col(i)) for i in range(len(self[0]))])

    def ref(self):
        """Gives the row-Eachelon form matrix."""
        if len(self) > len(self[0]):
            raise ValueError("matrix must have more columns than rows")
        cp = self.copy()
        for i in range(len(cp)):
            for j in range(i + 1,len(cp)):
                c = cp[j][i]/cp[i][i]
                for k in range(i,len(cp[0])):
                    cp[j][k] = cp[j][k] - c*cp[i][k]           
        return cp

    def rref(self):
        """Gives the reduced row-Eachelon form matrix."""
        cp = self.ref()
        for i in range(len(cp)):
            c = cp[i][i]
            for j in range(i,len(cp[0])):
                cp[i][j] = cp[i][j]/c
        for i in range(len(cp) - 1, -1, -1):
            for j in range(i - 1, -1, -1):
                c = cp[j][i]/cp[i][i]
                for k in range(i,len(cp[0])):
                    cp[j][k] = cp[j][k] - c*cp[i][k]
        return cp

    def without_top_and_col(self, ind):
        return Matrix([[self[j][i] for i in range(len(self[0])) if i != ind] for j in range(1,len(self))])

    def det(self):
        """Returns the determinant if the matrix is square."""
        if len(self) != len(self[0]):
            raise ValueError("Matrix must be square.")
        if len(self) == 1:
            return self[0][0]
        else:
            return sum(self[0][i]*-1*((i%2)*2 - 1)*self.without_top_and_col(i).det() for i in range(len(self)))

    def __abs__(self):
        return self.det()

    def inverse(self):
        if len(self) != len(self[0]) and self.det() != 0:
            raise ValueError("Must be a square matrix with determinant != 0!")
        cp = Matrix([[i for i in self[j]] + [i for i in Matrix.identity(len(self))[j]] for j in range(len(self))])
        cp = cp.rref()
        return cp[(slice(None),slice(len(self),len(cp[0])))]
