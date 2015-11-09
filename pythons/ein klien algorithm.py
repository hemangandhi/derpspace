from random import shuffle

#just a few cool algorithms with a focus on runtime.

def getXLargest(array, x):
    """
    Get an array of the x largest values in array.
    Uses selection sort, running with O(nx) for an array with n elements.
    Does not modify array, copying it instead. (Maybe making the runtime closer
    to O(nx + n).
    """
    temp = [i for i in array]
    index = 0
    while index < len(temp) - 1 and index < x:
        j = index + 1
        currMax = index
        while j < len(temp):
            if temp[j] > temp[currMax]:
                currMax = j
            j = j + 1
        max_val = temp[currMax]
        temp[currMax] = temp[index]
        temp[index] = max_val
        index = index + 1
    return temp[0:x]    

#print("X largest: get the first 10 elements range(100), shuffled")
#arr = [i for i in range(100)]
#shuffle(arr)
#print(getXLargest(arr,10))

def fst_no_rep(s):
    """
    Return the index of the first non-repeating element (string or list).
    Runs in O(n) using a set to store values and tracking the last item that
    was not a repeat. Returns len(s) if the item is not found.
    """
    i = len(s)
    t = set()
    r = i
    while i > 0:
        i = i - 1
        if s[i] not in t:
            r = i
            t.add(s[i])
    return r

#print("First non-repeat:")
#tests = ["she sells","sea shells", "on the sea shore arnt"]
#print("\n".join(i + " : " + str(fst_no_rep(i)) for i in tests))

def force_enum(iterator, clearer = False):
    """Returns a function to allow for indexing an iterator.
       The returned value:
       
       Takes an index and throws index out of bounds if index is greater than the iterator's
       length or less than 0.

       Promises constant-time access for all values before one called for
       ealier (maintains a memory of previous call values. All others are
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
    def wrap(f):
        def call(*args):
            """Here's how the voodoo works:

               This "call" replaces f, the function decorated.
               The first if is a simple though vital validator.
               The next checks if f can be applied and, if so,
               returns f(*args).
               Otherwise a partial function is returned. The partial
               takes any number of arguments so that calls can return other partial
               functions."""
            if len(args) not in arg_cts:
                raise TypeError("Invalid number of arguments")
            elif len(args) == arg_cts[len(arg_cts) - 1]:
                return f(*args)
            else:
                def part(*x):
                    passed = args + x
                    return call(*passed)
                return part
        return call
    return wrap

@partial_wrap(1,2,3)
def add(x, y, z):
    return x + y + z
    
