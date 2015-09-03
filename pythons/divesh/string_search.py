def find_str(key, string):
  low = 0
  high = len(string)
  mid = (low + high) //2
  while True:
    while low < high:
      if key[0] < string[mid]:
        high = mid
        mid = (low + high)//2
      elif key[0] == string[mid]:
        break
      else:
        low = mid
        mid = (low + high)//2

    if key[0] != string[mid]:
      return -1
    elif len(key) == 1:
      return mid
    elif len(key) > len(string) - mid:
      return -1
    elif key[1] != string[mid + 1]:
      low = mid
      high = len(string)
      mid = (low + high) // 2
    else:
      for i in range(2,len(key)):
        if key[i] != string[mid + i]:
          return -1
      else:
        return mid

def test_find(k, v):
  i = find_str(k ,v)
  if i > 0:
    return v[i: i + len(k)] == k
  else:
    return k not in v

test_find('abb','aaaaaaaaaaaab')
