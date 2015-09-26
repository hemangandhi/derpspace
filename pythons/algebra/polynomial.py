from functools import reduce
from math import log

def merge_dicts(dl, dr, func = lambda x,y: x + y):
    """Merge two dicts. Matching keys will have values determined by func."""
    r = dl.copy()
    for i in dr:
        if i in r:
            r[i] = func(r[i],dr[i])
        else:
            r[i] = dr[i]
    return r        

class Polynomial:
    """Represents a basic polynomial."""
    def validate(data):
        """Validates a polynomial returning the validated form. Returns None for invalid data."""
        if type(data) == Polynomial:
            return data
        elif type(data) == dict and all(type(data[i]) in [int, float] and type(i) in [int,float] for i in data):
            return {i : data[i] for i in data if data[i] != 0}
        elif type(data) in [int, float]:
            return {0: data}
        elif type(data) == str:
            return Polynomial.from_string(data, r_dict = True)
        else:
            return None
    def term_to_string(deg,cof):
        """Returns a string that represent the given term."""
        if deg == 0:
            return str(cof)
        elif deg == 1:
            if cof != 1:
                return str(cof) + "x"
            else:
                return "x"
        else:
            if cof != 1:
                return str(cof) + "x^" + str(deg)
            else:
                return "x^" + str(deg)
    def from_string(st, var = 'x', r_dict = False):
        """Converts a polynomial ax^n + ...
           var sets the variable to search for.
           r_dict determines whether to return a dictionary or Polynomial.
        """
        terms = st.replace(" ","").split("+")
        i = 0
        while i < len(terms):
            if '-' in terms[i]:
                m = terms[i].split('-')
                if m[0] == '':
                    m = m[1:]
                    m[0] = '-' + m[0]
                j = 0    
                while j < (len(m)):
                    if m[j][len(m[j]) - 1] == '^':
                        m = m[:j] + [m[j] + '-' + m[j + 1]] + m[j + 2:]
                    j = j + 1    
                        
                terms = terms[:i] + [m[0]] + ['-' + j for j in m[1:]] + terms[i + 1:]
            i = i + 1    
        
        cofs = []
        exps = []
        for i in terms:
            ind = i.find(var)

            cof = 0
            if ind < 0:
                cof = float(i)
            elif ind == 0:
                cof = 1.0
            elif i[:ind] == '-':
                cof = -1.0
            else:
                cof = float(i[:ind])

            exp = 0
            if ind < 0:
                exp = 0
            elif len(i[ind + len(var) + 1:]) == 0:
                exp = 1.0
            else:
                exp = float(i[ind + len(var) + 1:])

            if exp in exps:
                cofs[exps.index(exp)] += cof
            else:
                exps += [exp]
                cofs += [cof]
        r = {exps[i]: cofs[i] for i in range(len(exps))}
        if(r_dict):
            return r
        else:
            return Polynomial(r)

    def __init__(self, data):
        """Make a polynomial, validating the data."""
        data = Polynomial.validate(data)
        if data == None:
            raise TypeError("Data must be of type float or dict of float")
        else:
            self.data = data

    def copy(self):
        """Return a copy."""
        return Polynomial({i : self[i] for i in self})

    def __iter__(self):
        """Lets you iter over the exponenets present in the polynomial."""
        return iter(self.data)
    def __getitem__(self, key):
        """Get the coefficient of the given exponent."""
        if key in self.data:
            return self.data[key]
        else:
            return 0
    def __setitem__(self, key, val):
        """Set the coefficient of the given exponent."""
        if val == 0 and key in self:
            del self.data[key]
        elif val != 0:    
            self.data[key] = val

    def __repr__(self):
        """Get a string representation."""
        return " + ".join(Polynomial.term_to_string(i, self[i]) for i in self)
    def __str__(self):
        """Same as repr - gets a string representation."""
        return repr(self)

    def degree(self):
        """Get the highest degree in the polynomial."""
        return max(self)
        
    def __call__(self, x):
        """Evaluate the polynomial at x."""
        return sum(self[i]*x**i for i in self)
    def __add__(self, other):
        """Add with another polynomial (or int, dict or string representation)"""
        self = Polynomial.validate(self)
        other = Polynomial.validate(other)
        if other == None or self == None:
            raise TypeError("Other and self must be type polynomial, float or dict of float, not " + type(other))
        else:
            r = merge_dicts(self,other)
            return Polynomial({i:r[i] for i in r if r[i] != 0})
    def __mul__(self, other):
        """Multiply two polynomials"""
        other = Polynomial.validate(other)
        self = Polynomial.validate(self)
        if other == None or self == None:
            raise TypeError("Other and self must be type polynomial, float or dict of float, not " + type(other))
        return reduce(Polynomial.__add__,[{i + j: self[i]*other[j]} for i in self for j in other])

    def __eq__(self, other):
        """Compare two polynomials."""
        other = Polynomial.validate(other)
        if other == None:
            return False
        else:
            return all(self[i] == other[i] for i in other)
    
    def __radd__(self, other):
        return self + other
    def __rmul__(self, other):
        return self * other
    def __sub__(self, other):
        return self + -1 * other
    def __rsub__(self, other):
        return other + -1 * self
    def __pow__(self, exp):
        """Raise a polynomial to a non-negative integer exponent."""
        if type(exp) != int or exp < 0:
            raise TypeError("Exponent must be a non-negative integer")
        r = 1
        for i in range(exp):
            r = r * self
        return r

    def derivative(self):
        """Return the polynomial that is the derivative."""
        return Polynomial({i - 1: self[i]*i for i in self})
    def anti_derivative(self):
        """Return the anti-derivative. Returns a string with an ln for any 1/x."""
        p = {i + 1: self[i]/(i + 1) for i in self if i != -1}
        if -1 in self:
            return str(Polynomial(p)) + " + " + str(self[-1]) + "ln(x) + C"
        else:
            return Polynomial(p)
    def integral(self, a, b):
        """Get the definite integral from a to b. Not implemented for 1/x."""
        ad = self.anti_derivative()
        if type(ad) == str:
            raise ValueError("Cannot integrate 1/x.")
        else:
            return ad(b) - ad(a)
