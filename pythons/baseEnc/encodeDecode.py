#! /usr/bin/env python3

def iter_read(file_obj):
    c = file_obj.read(1)
    while c != '':
        yield c
        c = file_obj.read(1)

def write_64(enc_file, in_file, out_file):
    with open(enc_file, 'r') as enc:
        chars = enc.read()
    with open(in_file, 'r') as inp:
        with open(out_file, 'w') as out:
            carry = 0
            for cwc in iter_read(inp):
                out.write(chars[(ord(cwc) + carry) % len(chars)])
                carry = int((ord(cwc) + carry) / len(chars))
            while carry > 0:
                out.write(chars[carry % len(chars)])
                carry = int(carry / len(chars))

def read_64(enc_file, in_file, out_file):
    with open(enc_file, 'r') as enc:
        c2v = dict(map(lambda x: x[::-1], enumerate(enc.read())))
    with open(in_file, 'r') as inp:
        bi = sum(c2v[j] * len(c2v) ** i for i, j in enumerate(iter_read(inp)))
    with open(out_file, 'w') as out:
        while bi > 0:
            out.write(chr(bi % 256))
            yield bi%256
            bi = int(bi / 256)
