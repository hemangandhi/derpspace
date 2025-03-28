from typing import Iterator


def sieve_primes_til(n: int) -> Iterator[int]:
    sieve_bools = [False for i in range(2, n + 1)]
    sieve_i = 0
    while sieve_i < len(sieve_bools):
        if sieve_bools[sieve_i]:
            sieve_i += 1
            continue
        yield sieve_i + 2
        for i in range((sieve_i + 2) ** 2 - 2, len(sieve_bools), sieve_i + 2):
            sieve_bools[i] = True
        sieve_i += 1


def int_log(x: int, b: int) -> int:
    pows = []
    p = b
    while p <= x:
        pows.append(p)
        p *= p
    log = 0
    for i, v in enumerate(reversed(pows)):
        if v <= x:
            log += 2 ** (len(pows) - i - 1)
            x //= v
    return log


def size_n_subsets[T](s: list[T], n: int) -> Iterator[list[T]]:
    pass

# It's https://projecteuler.net/problem=72
def gcd(a: int, b: int) -> int:
    while a != 0:
        a, b = b % a, a
    return b

def n_frac(n: int):
    return sum(1 for x in range(1, n + 1) for y in range(1, x) if gcd(x, y) == 1)
