# Being even more interested in https://xkcd.com/2835/

from typing import Protocol, TypeVar, Optional, Union
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

    @cached_property
    def permutation(self):
        indices = list(range(len(self.digits) + 1))
        for digit, cycle_size in zip(self.digits[::-1], range(2, len(self.digits) + 2)):
            cycle = indices[:cycle_size]
            indices = cycle[:digit]  + cycle[digit:]  + indices[cycle_size:]
        return indices

    @classmethod
    def from_int(cls, num: int, representation: DigitConverter[D]) -> FactorialInt[D]:
        digits = []
        while num > 0:
            next_digit = representation.to_digit(num % (len(digits) + 2))
            if next_digit is None:
                raise ValueError(f"{num % (len(digits) + 2) is too large to be represented.")
            num = num // len(digits) + 2
            digits.append(next_digit)
         cls(digits[::-1], representation)

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
