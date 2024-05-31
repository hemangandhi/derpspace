# Being even more interested in https://xkcd.com/2835/

from typing import Protocol, TypeVar, Optional, Callable
from collections.abc import Iterable
import functools


D = TypeVar('D')
T = TypeVar('T')


def yield_factorials(start: int = 1) -> Iterable[int]:
    n = start
    acc = n
    while True:
        yield acc
        n += 1
        acc *= n


class DigitConverter(Protocol[D]):
    def to_digit(self, num: int) -> Optional[D]:
        pass
    def from_digit(self, digit: D) -> Optional[int]:
        pass


class UseDecimalWithinSlots:
    def to_digit(self, num: int) -> Optional[int]:
        return num
    def from_digit(self, digit: int) -> Optional[int]:
        return digit


class FactorialInt:
    digits: list[D]
    digit_representation: DigitConverter[D] = UseDecimalWithinSlots()

    def __init__(self, digits: list[D], representation: DigitConverter[D]):
        self.digits = digits
        self.digit_representation = representation

    @functools.cached_property
    def permutation(self):
        indices = list(range(len(self.digits) + 1))
        for digit, cycle_size in zip(self.digits[::-1], range(2, len(self.digits) + 2)):
            cycle = indices[:cycle_size]
            indices = cycle[:digit]  + cycle[digit:]  + indices[cycle_size:]
        return indices

    @classmethod
    def from_int(cls, num: int, representation: DigitConverter[D] = UseDecimalWithinSlots()) -> 'FactorialInt[D]':
        digits = []
        while num > 0:
            next_digit = representation.to_digit(num % (len(digits) + 2))
            if next_digit is None:
                raise ValueError(f"{num % (len(digits) + 2)} is too large to be represented.")
            num = num // (len(digits) + 2)
            digits.append(next_digit)
        return cls(digits[::-1], representation)

    def __int__(self) -> int:
        acc = 0
        for digit, base in zip(self.digits[::-1], yield_factorials(2)):
            num = self.representation.from_digit(digit)
            if num is None:
                raise ValueError(f"{digit} could not be converted to an int.")
            if num >= base:
                raise ValueError(f"{digit} (converted to {num}) is too large.")
            if num < 0:
                raise ValueError(f"{digit} (converted to {num}) is negative.")
            acc += base * digit
        return acc

    def __call__(self, items: list[T]) -> list[T]:
        if len(items) < 2:
            return items[:]
        if len(items) < len(self.digits):
            return [items[i] for i in self.permutation[:len(items)]]
        return [items[i] for i in self.permutation] + items[len(self.digits):]

    def __str__(self) -> str:
        return f"{self.digits} ({int(self)})"


def ensure_prompt(prompt: str, validate: Callable[[str], Optional[str]]):
    response = input(prompt)
    while (error := validate(response)) is not None:
        print(error)
        response = input(prompt)
    return response


def run_prompt():
    state: dict[int, FactorialInt[int]] = {}
    ctr: int = 0

    def check_command(command: str) -> Optional[str]:
        fn, arg_str = command[0], command[1:]
        if fn not in ['!', 'S', 'q']:
            return f"Cannot handle function {fn}"
        if fn == 'q':
            return None
        if not arg_str.isdigit():
            return f"Cannot handle arg {arg_str}"
        arg = int(arg_str)
        if fn == '!':
            state[ctr] = FactorialInt.from_int(arg)
            return None
        perm = state.get(arg)
        if perm is None:
            keys = ", ".join(map(str, state.keys()))
            return f"Value {arg} has not been computed, try one of {keys}"
        print(f"Permutation of {perm} is {perm.permutation}")
        state[ctr] = perm

    print("! = int->factorial base, S = get permutation of nth, q = quit")
    while ensure_prompt(f"Enter command {ctr} > ", check_command) != "q":
        ctr += 1
        print("! = int->factorial base, S = get permutation of nth, q = quit")


if __name__ == "__main__":
    run_prompt()
