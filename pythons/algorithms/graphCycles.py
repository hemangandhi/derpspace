from random import sample, randint
from hemanUtils import DeferImpl, rev_dict
from time import time

def millis():
    return int(round(time()*1000))

def rlen(v):
    return range(len(v))

def rotate(l, v):
    return l[v:] + l[:v]

@DeferImpl(['__len__', '__hash__', '__iter__', '__repr__', '__contains__'])
class Loop:
    __defer__ = rev_dict({'l':['__len__', '__hash__', '__iter__', '__repr__', '__contains__']})
    def __init__(self, it):
        idx, v = min(enumerate(it), key = lambda x: x[1])
        self.l = rotate(tuple(it), idx)
    def __getitem__(self,key):
        if type(key) == int:
            return self.l[key%len(self)]
        elif type(key) == slice:
            if key.start == None:
                start = 0
            else:
                start = key.start
            if key.step == None:
                step = 1
            else:
                step = key.step
            st = key.stop
            if st == None:
                st = len(self)
            if st < key.start and step > 0:
                st += len(self)
            return tuple(self[i] for i in range(start,st,step))    
        else:
            raise TypeError("Invalid key!")

    def __eq__(self, other):
        if len(self) != len(other):
            return False

        find = other[0]
        pos = -1
        for i in rlen(self):
            if self[i] == find:
                pos = i
                break
        else: 
            return False
        
        return all(self[pos + c] == other[c] for c in rlen(self))

    def __add__(self, tup):
        """
        Add a tuple to this loop assuming the first 
        is where in the loop to start and the end is where
        to stop overwritting.
        """
        p1, p2 = -1, -1
        val1, val2 = tup[0], tup[-1]
        for i in rlen(self):
            if p1 < 0 and self[i] == val1:
                p1 = i
            if p2 < 0 and self[i] == val2:
                p2 = i
            
            if p1 >= 0 and p2 >= 0:
                break
        else:
            raise ValueError("Cannot add disjoint loops!")

        return Loop(tup + self[p2 + 1: p1])

    def can_add(self, tup):
        return tup[0] in self and tup[-1] in self
    

def connected(node, graph, acc, d):
    nbhs = graph[node]
    graph[node] = {}
    for i in nbhs:
        for j in rlen(acc):
            if acc[j] == i:
                a = Loop(acc[j:])
                d.add(a)
                break
        else:
            tmp = d.copy()
            for k in tmp:
                if k.can_add(acc + (i,)):
                    d.add(k + (acc + (i,)))
            connected(i, graph, acc+(i,), d)

def all_cycles(graph):
    d = set()
    for node in graph:
        connected(node, graph.copy(),(node,), d)
    return d

def gen_graph(num_nodes, num_edges):
    nds = {chr(i) for i in range(ord('A'), ord('A') + num_nodes)}
    rd = dict()
    for i in nds:
        n = randint(0, num_edges)
        rd[i] = set(sample(nds - {i}, min(n, len(nds) - 1)))
        num_edges -= n
    return rd

def test_cycles(tc, nds, edges):
    for i in range(tc):
        g = gen_graph(nds, edges)
        print("Graph:")
        print(g)
        print("-"*50)
        m = millis()
        print(all_cycles(g))
        print("Time:", millis() - m)
        print('-'*50)

