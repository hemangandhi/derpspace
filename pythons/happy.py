#! /usr/bin/env python3

def sum_dig(n):
    return sum(int(i) * int(i) for i in str(n))

def is_happy_mk():
    hap = set([1])
    unhap = set()
    def is_happy(n):
        nonlocal hap, unhap
        #A few elementary cases...
        if n <= 0:
            raise ValueError("Out of domain")
        elif n in hap:
            return True
        elif n in unhap:
            return False

        #we must be sure not to re-examine old evidence.
        checked = set()
        i = sum_dig(n)
        while True:
            if i in checked or i in unhap:
                unhap |= checked
                return False
            elif i in hap:
                hap |= checked
                return True
            else:
                checked.add(i)
                i = sum_dig(i)

    return is_happy

is_happy = is_happy_mk()
