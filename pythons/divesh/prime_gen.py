#works in python 2 and 3.

def primesUntil(condition):
    """Yields primes until condition is met. condition must be a
       callable that accepts a list argument."""
    mem = [2]
    while not condition(mem):
        i = mem[len(mem) - 1]
        yield i
        i = i + 1 #saves one loop through mem.
        while any(i % j == 0 for j in mem):
            i = i + 1
        mem = mem + [i]


def yieldPrimes(count_of_primes):
    """Yields the number of primes desired."""
    #'yield from' would be quicker than 'return' in py3...
    return primesUntil(lambda lst: len(lst) >= count_of_primes)

def primesLessThan(num):
    """Gets the primes less than num. num can be any integer."""
    return primesUntil(lambda lst: lst[len(lst) - 1] >= num)

def isPrimeWithSieve(num):
    """See if num is prime. (Slow!)"""
    return not any(num % j == 0 for j in primesLessThan(num))

def isPrime(num):
    """Sees if a number is prime. (Faster than above - is O(sqrt(n) + 1).)"""
    return not any(num % j == 0 for j in range(2,(num**.5) + 1))
