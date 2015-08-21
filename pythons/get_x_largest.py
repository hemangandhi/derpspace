from random import shuffle

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

arr = [i for i in range(100)]
shuffle(arr)
print(getXLargest(arr,10))
