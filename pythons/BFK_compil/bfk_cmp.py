#! /usr/bin/env python3
import re

def print_ret(v):
    print(v)
    return v

def clear_news(st):
    st_ind = 0
    if st.startswith('\n'):
        st_ind = 1
    en_ind = len(st)
    if st.endswith('\n'):
        en_ind -= 1
    return st[st_ind: en_ind]

bfk_chars = {">": True, "<": True, "+": True, "-": True, ".": False, ",": False, "[": False, "]": False}

def lex(bfk_str, w_mismatch):
    l_l = ''
    l_c = 0
    par_c = 0
    for i in bfk_str:
        if i in bfk_chars:
            if i == '[':
                par_c += 1
            elif i == ']':
                par_c -= 1

            if par_c < 0:
                w_mismatch(par_c)
                break

            if l_l == '':
                l_l = i
                l_c = 1
            elif l_l != i or not bfk_chars[i]:
                if l_l != '':
                    yield (l_l, l_c)
                if not bfk_chars[i]:
                    l_l = ''
                    l_c = 0
                    yield (i, 1)
                else:
                    l_l = i
                    l_c = 1
            else:
                l_c += 1

    while par_c > 0:
        yield (']', 1)
        par_c -= 1
    w_mismatch(1)

def bfk_map_2_dict(map_str, err_han):
    comps = filter(lambda x: len(x) > 0 and x != '\n' and not all(i == '=' for i in x), 
            re.compile("^!(?P<eqs>=+)(?P<tag>.+)(?P=eqs)!$", re.MULTILINE).split(map_str))
    comps = print_ret(list(comps))
    rv = {i[0]: clear_news(i[1]) for i in zip(*([iter(comps)]*3)) 
            if i[2] == '\\' + i[0]}
    if all(i in rv for i in bfk_chars):
        if "init" not in rv:
            rv["init"] = ""
        if "indent" not in rv:
            rv["indent"] = ""
        if "no-coll" not in rv:
            rv["no-coll"] = False
        if "suffix" not in rv:
            rv["suffix"] = ""
        return rv
    else:
        err_han(rv)

def comp_bfk(in_str, map_in, lex_w):
    gen = map_in["init"]
    indent = map_in["indent"]

    for i in lex(in_str, lex_w):
        val = '\n'.join(indent + j for j in map_in[i[0]].split('\n'))
        print(val)
        if bfk_chars[i[0]] and not map_in["no-coll"]:
            gen += val.format(i[1])
        else:
            for j in range(i[1]):
                gen += val
        
        if i[0] == '[':
            indent += map_in["indent"]
        elif i[0] == ']':
            indent = indent[1:]
    return gen + "\n" + map_in["suffix"]

def main(in_p, out_p, map_p):
    comp = True
    def err_han(v):
        nonlocal comp
        print("Incomplete map file, read: ", v)
        print("Missing: ", set(bfk_chars) - set(v))
        comp = False

    def w_mismatch(m):
        nonlocal comp
        if m > 0:
            print("Adding ] at the end")
        else:
            print("Too many ]!")
            comp = False

    with open(in_p) as inp:
        with open(map_p) as mapp:
            map_i = bfk_map_2_dict(mapp.read(), err_han)
            if not comp: return
            out = comp_bfk(inp.read(), map_i, w_mismatch)
            if not comp:
                return
            with open(out_p, 'w') as outp:
                outp.write(out)

if __name__ == "__main__":
    from sys import argv
    main(*(argv[1:]))
