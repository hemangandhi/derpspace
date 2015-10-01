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

print("X largest: get the first 10 elements range(100), shuffled")
arr = [i for i in range(100)]
shuffle(arr)
print(getXLargest(arr,10))

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

print("First non-repeat:")
tests = ["she sells","sea shells", "on the sea shore arnt"]
print("\n".join(i + " : " + str(fst_no_rep(i)) for i in tests))
