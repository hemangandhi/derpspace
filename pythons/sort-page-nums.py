#! /usr/bin/env python

import os
import os.path
import re

def padder_at_len(len_to_reach, pad_with='0'):
    def pad_str(str_match_tuple):
        s, match = str_match_tuple
        num_pad = len_to_reach - len(match[1])
        replacement = num_pad * pad_with + match[1]
        print(s[:match.start(1)] + replacement + s[match.end(1):], 'is replacement with match [', match.start(1), ':', match.end(1), ']')
        return s, s[:match.start(1)] + replacement + s[match.end(1):]
    return pad_str

def find_and_0_pad_nums(strings_with_nums, num_getting_regex='.+(\\d+).+$'):
    compiled_regex = re.compile(num_getting_regex)
    str_num_tuple = map(lambda x: (x, compiled_regex.match(x)), strings_with_nums)
    strs_that_match = list(filter(lambda x: x[1], str_num_tuple))
    len_to_reach = max(map(lambda x: len(x[1][1]), strs_that_match))
    return map(padder_at_len(len_to_reach), strs_that_match)

def ls_then_mv(dir_path='.', regex='.+(\\d+).+$'):
    for src, dest in find_and_0_pad_nums(map(os.fspath, os.listdir(dir_path)), regex):
        src_path = os.path.join(dir_path, src)
        dest_path = os.path.join(dir_path, dest)
        print('doing mv', src_path, dest_path)
        os.rename(src_path, dest_path)

if __name__ == "__main__":
    from sys import argv
    args = {'-d': '.', '-r': '.+?(\\d+).+$'}
    for arg in argv[1:]:
        if len(arg) >= 2:
            print('ignoring unclear arg', arg)
        if arg[:2] not in args:
            print('ignoring undefined arg', arg)
        args[arg[:2]] = arg[2:]
    ls_then_mv(args['-d'], args['-r'])
