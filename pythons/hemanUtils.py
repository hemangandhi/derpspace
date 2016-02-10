"""Some utilities for elegance."""

#from ein kliene algorithm...

def force_enum(iterator, clearer = False):
    """Returns a function to allow for indexing an iterator.
       The returned value:
       
       Takes an index and throws index out of bounds if index is greater than the iterator's
       length or less than 0.

       Promises constant-time access for all values before one called for
       ealier (maintains a memory of previous call values). All others are
       n-time.

       The clearer, if true, will also provide a function to clear the memory.
       This function will return the value of the memory. A 2-element tuple will be returned:
       (index getter, clearer).
    """
    iterator = iter(iterator)
    mem = []
    def get_index(ind):
        """Gets an index within the iterator."""
        nonlocal mem
        if ind < 0:
            raise ValueError("Index out of bounds.")
        elif ind < len(mem):
            return mem[ind]
        else:
            try:
                for i in range(len(mem),ind + 1):
                    mem += [next(iterator)]
                return mem[ind]
            except StopIteration:
                raise ValueError("Index out of bounds.")
    if not clearer:        
        return get_index
    else:
        def clear():
            nonlocal mem
            t = mem
            mem = []
            return t
        return (get_index, clear)

def partial_til_cond(cond, err = lambda x: False):
    """Decorator for the use and creation of automatically made partial functions.

       If the condition is met, the original function is called. Else,
       the passed arguments are stored in another function. That function
       may later call the original function (once the condition is met).

       Cond is expected to accept a variable and return a bool. The variable is
       the list of passed in parameters.

       Err is passed in the same parameter list, but will cause this decorator to
       raise a TypeError if met. Defaults to never raising an error.

       See below for an application of ths.
       """
    def wrap(f):
        def call(*args):
            if err(args):
                raise TypeError("Error in arguments")
            elif cond(args):
                return f(*args)
            else:
                return lambda *x: call(*(args + x))
        return call
    return wrap

def partial_wrap(*arg_cts):
    """Decorator that allows for Haskell-like partial functions. (No kw-arguments.)
       Pass in the number of arguments as the last item, please.
       pass in list(range( number of args + 1)) for any legal number of arguments to be used.
       Also, no arbitrary number of arguments (discerning what to return would not be possible).

       Examples:

       #example 1
       @partial_wrap(1,2)
       def add(x, y):
           return x + y

       inc = add(1)
       inc(2) #returns 3

       #example 2:
       @partial_wrap(1,2,3)
       def add(x, y, z):
           return x + y + z

       a = add(2)
       a(8)(2) #12
       inc = a(-1)
       inc(6) #7
       a(8,2) #12
       """
    return partial_til_cond(lambda x: len(x) == arg_cts[len(arg_cts) - 1], lambda x: len(x) not in arg_cts)
    
@partial_til_cond(lambda x: not hasattr(x[len(x) - 1], '__call__'))
def thread(*v):
    """
       Allows the threading of various functions.

       >>>thread(list, range, 8)
       [0, 1, 2, 3, 4, 5, 6, 7]

       If tuples are returned in the middle, the
       tuple is unpacked and passed down.

       This goes from right-to-left in precedence,
       consistent with notation (ie. thread(f, g, x) == f(g(x))).

       If the last argument is callable, returns the composition of
       all passed in values instead. thread(f, g) == lambda x: f(g(x))
    """
    j = v[len(v) - 1]
    for i in v[1::-1]:
        if type(j) == tuple:
            j = i(*j)
        else:
            j = i(j)
    return j        

def pk(f):
    """Packs arguments and calls f on resulting list.

       Useful to simplify the calling of compositions made by thread."""
    def w(*a):
        return f(a)
    return w

rlen = thread(range, len)
lfilt = pk(thread(list, filter))
len_filt = pk(thread(len, lfilt))

#same as interface_meta, but better.
class Interface:
    """A class decorator to check if a class implements every
       function listed.

       Allows for set operations (|, & and -) with the functions
       checked for."""
    def __init__(self, funcs):
        self.funcs = set(funcs)
    def __or__(self, o):
        return type(self)(*(self.funcs | o.funcs))
    def __and__(self, o):
        return type(self)(*(self.funcs & o.funcs))
    def __sub__(self, o):
        return type(self)(*(self.funcs - o.funcs))
    def __iter__(self):
        return iter(self.funcs)
    def __contains__(self, f):
        return f in self.funcs
    #A tad depressing that the last 4 are excellent use-cases of DeferImpl.
    #__defer__ = rev_dict({'funcs':['__or__','__and__','__sub__','__iter__']})
    def __call__(self, o_class):
        if all(hasattr(o_class, i) and hasattr(getattr(o_class, i),'__call__') for i in self.funcs):
            return o_class #close to interface_meta.interface.validate...
        else:
            raise TypeError("Must implement " + str(self.funcs))
    def __repr__(self):
        return type(self).__name__ + '(' + str(self.funcs) + ')'
    def __str__(self):
        return repr(self)

def mk_defer(di, fname, attr, ct = False):
    """Makes a deferrer for fname,
       deferring to attr. Adds this
       to di as di[fname].

       If ct is a type, passed the
       result to ct's constructor."""
    def defered(s, *a):
        b = []
        for j in a:
            if type(j) == type(s):
                b += [getattr(j, attr)]
            else:
                b += [j]     
        r = getattr(getattr(s, attr), fname)(*b)
        if type(ct) == type:
            return ct(r)
        elif ct == '':
            return type(s)(r)
        else:
            return r
    di[fname] = defered

class DeferImpl(Interface):
    """Defers the implementation of an interface to
       the specified field.

       Decorated classes are expected to have:
       __defer__ = {"function to defer's name": "field to defer to",...}
       Make the value a tuple: "(field, return type)" and the defered
       implentation will return the type provided. Set return type to ''
       to return the type being implemented."""
    def __init__(self, funcs):
        Interface.__init__(self, funcs)
    def __call__(self, cls):
        if not hasattr(cls, '__defer__'):
            raise TypeError("Class must have an __defer__")
        defer = cls.__defer__
        di = {i: cls.__dict__[i] for i in cls.__dict__ if i not in ['__defer__', '__dict__']}
        for i in defer:
            if i not in di:
                if type(i) == str:
                    mk_defer(di, i, defer[i])
                else:
                    if type(i[1]) != type:
                        mk_defer(di, i[0], defer[i], cls)
                    else:    
                        mk_defer(di, i[0], defer[i][0], i[1])               
        return Interface.__call__(self, type(cls.__name__, cls.__bases__, di))        

def rev_dict(d):
    """Reverse a dictionary of lists.

       Useful for a decorated class' __defer__.

       rev_dict({'field':['f1','f2','f3']}) gives:
       {'f1': 'field', 'f3': 'field', 'f2': 'field'}"""
    r = {}
    for i in d:
        for j in d[i]:
            r[j] = i
    return r

    
@DeferImpl(['__getitem__','__delitem__','__iter__','__contains__', '__len__'])
class mem_wrap:
    __defer__ = rev_dict({'mem':['__getitem__','__delitem__','__iter__','__contains__', '__len__']})
    def __init__(self, g):
        self.f = g
        self.mem = {}
    def __call__(self, *args):
        if args in self:
            return self[args]
        else:
            self.mem[args] = self.f(*args)
            return self[args]
    def clear_cache(self):
        r = self.mem
        self.mem = {}
        return r

def wrap_last():
    """
    Use this decorator as a partial-wrap that wraps until an callable is passed last.
    This is useful for creating decorators that accept arguments, as no nested decorator
    is needed.
    """
    return partial_til_cond(lambda x: hasattr(x[-1],"__call__"))
