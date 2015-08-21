

class KenKenMat:
    FLAG_VALUE = 0

    def make_by_mat(mat):
        if type(mat) == list and all(type(i) == list and len(i) == len(mat) for i in mat):
            to_ret = KenKenMat(len(mat))
            for i in range(len(mat)):
                for j in range(len(mat)):
                    if mat[i][j] != KenKenMat.FLAG_VALUE:
                        to_ret[(j,i)] = mat[i][j]
            return to_ret
        else:
            raise ValueError('Matrix passed in must be a square list of lists.')
    
    def __init__(self,dim):
        self.pts = {'dim':dim}
    def get_dim(self):
        return self.pts['dim']
    def __len__(self):
        return self.get_dim()
    def get_flag(self):
        return KenKenMat.FLAG_VALUE

    def to_mat(self):
        to_ret = [[self.get_flag() for i in range(len(self))] for j in range(len(self))]
        for i in self.pts:
            if type(i) != tuple:
                continue
            to_ret[i[1]][i[0]] = self.pts[i]
        return to_ret
    def __repr__(self):
        to_r = ""
        for i in self.to_mat():
            to_r += i.__repr__() + '\n'
        return to_r    

    def bounds_chk(self, *x, throw=False):
        is_in = all(0 <= int(i) < len(self) for i in x)
        if not is_in and throw:
            raise IndexError('Invalid location or value.')
        else:
            return is_in
    def is_valid_key(self, key, throw=False):
        val = type(key) == tuple and len(key) == 2
        if not val and throw:
            raise KeyError('{} is not a valid key! Use tuple(x, y).'.format(key))
        else:
            return val and self.bounds_chk(*key, throw=throw)

    def __getitem__(self, key):
        self.is_valid_key(key, True)
        if key in self.pts:
            return self.pts[key]
        else:
            return self.get_flag()
    def __setitem__(self, key, val):
        self.is_valid_key(key,True)
        self.bounds_chk(val - 1,throw=True)
        self.pts[key] = val
    def __delitem__(self, key):
        self.is_valid_key(key, True)
        del self.pts[key]
        
    def __add__(self, other):
        if type(other) == KenKenMat:
            if len(other) > len(self):
                return other.__add__(self)
            to_ret = KenKenMat(len(self))
            for i in self.pts:
                if self.is_valid_key(i):
                    to_ret[i] = self[i]
            for i in other.pts:
                if other.is_valid_key(i):
                    if to_ret[i] != KenKenMat.FLAG_VALUE:
                        raise ValueError('Could not merge matrices due to conflict at ({}, {}): {} vs {}'.format(i[0],i[1],to_ret[i],other[i]))
                    else:
                        to_ret[i] = other[i]
            return to_ret            
        elif type(other) == list:
            return self.__add__(KenKenMat.make_by_mat(other))
        else:
            raise NotImplementedError('Addition is not supported for this type!')

    def get_poss(self, x, y):
        self.bounds_chk(x, y, throw=True)
        full = set(range(1,self.pts['dim'] + 1))
        for i in self.pts:
            if not self.is_valid_key(i):
                continue
            elif (i[0] == x or i[1] == y) and self.pts[i] in full:
                full.remove(self.pts[i])
        return full

