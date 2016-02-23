#from random import shuffle

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

#force_enum, partial_til_cond, partial_wrap and thread have moved to hemanUtils
import hemanUtils

@hemanUtils.DeferImpl(['__mul__','__rmul__','__str__','__add__','__repr__', '__sub__','__isub__', '__iadd__'])
class Char:    
    def __init__(self, v):
        if type(v) == str:
            if len(v) != 1:
                raise ValueError("String length must be 1!")
            else:
                self.v = v
                self.o = ord(v)
        elif type(v) == int:
            self.v = chr(v)
            self.o = v
        elif type(v) == Char:
            self.v = v.v
            self.o = v.o
        else:
            raise TypeError("Only pass in int, str and Char types.")

    def __add__(self, o):
        if type(o) == Char:
            return Char(o.o + self.o)
        elif type(o) == int:
            return Char(self.o + o)
        elif type(o) == str:
            return self.v + o.v
        else:
            raise TypeError("Cannot add with " + type(o) + ". Only pass strs, ints and Chars.")

    def __radd__(self, o):
        return self.__add__(o)

    __defer__ = hemanUtils.rev_dict({'v':['__mul__','__rmul__','__str__','__repr__'],
                                    'o':[('__sub__', ''), ('__isub__', ''), ('__iadd__', '')]})
    
        
#A wrapper to store recent function calls is in hemanUtils

#A quick method to return all the permutations of dims*range(start, stop)
#Useful to find all adjacent points from a point in dims dimensions.
def n_dims(dims, start, stop):
	if dims == 1:
		return list(map(lambda x: [x], range(start, stop)))
	else:
		p = n_dims(dims - 1, start, stop)
		a = []
		for i in range(start, stop):
			a += [j + [i] for j in p]
		return a

#R-like vectorize wrapper
#Warning: Only pass 1 argument when wrapping. Second will be passed by python! (See wrap_last.)
@hemanUtils.wrap_last()
def auto_map(arg_c, f):
    def ret(*args):
        if len(args) != arg_c:
            raise TypeError("Invalid number of arguments!")
        elif hasattr(args[arg_c - 1], "__iter__"):
            bef = args[:arg_c - 1]
            return map(lambda x: f(*(bef + (x,))), args[arg_c - 1])
        else:
            return f(*args)
    return ret

def all_comb(l, e):
    if e == 0 or len(l) < e:
        return []
    elif len(l) == e:
        return [l]
    else:
        r = all_comb(l[1:], e)
        le = list(map(lambda x: [l[0]] + x, all_comb(l[1:], e - 1)))
        return r + le

def even_odd_partition(arr):
    n_o = sum(map(lambda x: x % 2 == 1, arr))
    i = 0
    j = n_o
    while i < n_o and j < len(arr):
        if arr[i] % 2 == 0 and arr[j] % 2 == 1:
            arr[i], arr[j] = arr[j], arr[i]
            i = i + 1
            j = j + 1
        else:
            if arr[i] % 2 == 1:
                i = i + 1
            if arr[j] % 2 == 0:
                j = j + 1
    return arr
