class interface(type):
    def __new__(self, name, sups, attrs):
        if any(i not in attrs or not hasattr(attrs[i], '__call__') for i in self.funcs):
            raise TypeError("All methods not implemented!")
        else:
            return type.__new__(self, name, sups, attrs)
    def validate(self, o_class):
        return all(hasattr(o_class, i) and hasattr(getattr(o_class, i),'__call__') for i in self.funcs)
            

def make_interface(name, *funs):
    return type(name,(interface,),{'funcs':list(funs)})

class defer_implementation(interface):
    def __new__(slf, name, sups, attrs):
        if '__defer__' not in attrs:
            raise ValueError("Provide the name of the instance field, please!")
        defer = attrs['__defer__']
        del attrs['__defer__']
        for i in slf.funcs:
            if i not in attrs:
                def n(self, *a, i = i):
                    return getattr(getattr(self, defer[i]), i)(*a)
                attrs[i] = n
        return interface.__new__(slf, name, sups, attrs)

def make_deferrer(name, *funcs):
    return type(name, (defer_implementation,), {'funcs':list(funcs)})

def merged_by(name, merger, *ints):
    return merger(name, *list(set(sum(map(lambda x: x.funcs, ints), []))))

def merge_interfaces(name, *ints):
    return merged_by(name, make_interface, *ints)

def merge_deferrers(name, *ints):
    return merged_by(name, make_deferrer, *ints)

def rev_dict(d):
    r = dict()
    for i in d:
        for j in d[i]:
            r[j] = i
    return r        

indexable = make_interface('indexable','__getitem__')
index_can_set = make_interface('index_can_set', '__setitem__')
index_can_remove = make_interface('index_can_remove','__delitem__')

mutable_indexable = merge_interfaces('mutable_indexable',indexable, index_can_set, index_can_remove)

list_defer = merge_deferrers('list_defer',mutable_indexable)

comparable_int = make_interface('comparable_int','__eq__','__lt__','__gt__')
comparable_defer = merge_deferrers('comparable_defer',comparable_int)

class Card(metaclass=comparable_defer):
    __defer__ = rev_dict({'face':comparable_defer.funcs})
    def __init__(self, face, suit):
        self.face = face
        self.suit = suit
