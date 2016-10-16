#! /usr/bin/env python3

"""
DATA REPRESENTATION
{ "non-terminal": {("rule", "1", "symbol", "by", "symbol")...} ...}

If a symbol is not in the keys of the dictionary, it is assumed to be a terminal.
"""

def firsts_from_helper(symbol, rules, epsilon = "eps", ps = dict()):
    if symbol not in rules: #the first set of a terminal is {terminal}...
        ps[symbol] = set([symbol])
        return ps
    ps[symbol] = set() #otherwise...
    if (epsilon,) in rules[symbol]: #epsilon is there only if the set can be re-written as such.
        ps[symbol] |= set([epsilon])
    for rule in rules[symbol]:
        for indx, sym in enumerate(rule):
            if sym not in ps: #recurse if we haven't already...
                firsts_from_helper(sym, rules, epsilon, ps)
            ps[symbol] |= ps[sym] - set([epsilon]) #If A := B, FIRST(B) is in FIRST(A)
            if epsilon not in ps[sym]:
                break
            elif indx == len(rules) - 1: # If A := BCDE and epsilon is in FIRST(BCDE), epsilon is in FIRST(A).
                ps[symbol] |= set([epsilon])
        else: #if the loop is never broken, the last symbol had an eps, which means this symbol has an eps.
            ps[symbol] |= set([epsilon])
    return ps

def firsts_from(symbol, rules, epsilon = "eps", ps = dict()):
    return firsts_from_helper(symbol, rules, epsilon, ps.copy())

print(firsts_from("<S>", {"<S>": [("<A>", "<S>", "<B>"), ("eps",)], "<A>": [("a",), ("eps",)], "<B>": [("b"), ("eps",)]}))
print(firsts_from("<S>", {"<S>" : [("X", "y")], "X" : [("<S>", "x"), ("z")]}))
print(firsts_from("<S>", {"<S>" : [("X", "y")], "X" : [("<T>", "x"), ("z")], "<T>": [("<S>", "q"), ('eps',)]}))
print(firsts_from("<X>", {"<X>": [("<D>", "<Y>"), ("eps",)], "<D>": [("eps",)], "<Y>": [("<X>",)]}))
print(firsts_from("<A>", {"<A>": [("<B>", "<C>")], "<B>": [("<C>",), ("eps",)], "<C>": [("<A>",), ("c",)]}))
print(firsts_from("<A>", {"<A>": [("<B>", "<C>")], "<B>": [("<C>",), ("eps",)], "<C>": [("<A>",), ("eps",)]}))

"""
Need to prove (if this is true, the above should always work):
for all s where there is a cycle: eps is in first(s, r) iff there is a k in the cycle such that k := eps.

Simple idea of proof: if k := v and v :=* eps, v breaks the cycle, for some k in the cycle - since nothing
should appear after eps.
"""
