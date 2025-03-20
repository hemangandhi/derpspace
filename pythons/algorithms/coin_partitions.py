from functools import lru_cache, wraps


def wrap_log(fn):
    class Wrapper:
        _indent: str

        def __init__(self):
            self._indent = ""

        @wraps(fn)
        def __call__(self, *args):
            print(f"{self._indent} Call {repr(fn)} with {args}")
            self._indent += "  "
            try:
                r = fn(*args)
            except Exception as e:
                self._indent = self._indent[:-2]
                print(f"{self._indent} Call: fn{args} raised {e}")
                raise e
            else:
                self._indent = self._indent[:-2]
                print(f"{self._indent} Call: fn{args} returns {r}")
            return r

    return Wrapper()


@lru_cache
@wrap_log
def num_decreasing_partitions_of_parts(n: int, parts: int, upper: int) -> int:
    if parts > n:
        return 0
    if parts == n:
        return 1
    if parts * upper < n:
        return 0
    if upper == 1:
        return 1
    return sum(
        num_decreasing_partitions_of_parts(n - i, parts - 1, min(i, n - i))
        for i in range(1, upper + 1)
    )


def num_decreasing_partitions(n: int) -> int:
    return sum(num_decreasing_partitions_of_parts(n, i, n) for i in range(1, n + 1))
