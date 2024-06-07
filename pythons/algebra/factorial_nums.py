# Being even more interested in https://xkcd.com/2835/

from typing import Protocol, TypeVar, Optional, Callable, Union
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
        for digit, cycle_size in zip(self.digits, range(2, len(self.digits) + 2)):
            cycle = indices[:cycle_size]
            num = self.digit_representation.from_digit(digit)
            if num is None:
                raise ValueError(f"{digit} could not be converted to an int.")
            if num < 0:
                raise ValueError(f"{digit} (converted to {num}) is negative.")
            indices = cycle[num:] + cycle[:num] + indices[cycle_size:]
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
        return cls(digits, representation)

    def __int__(self) -> int:
        acc = 0
        for digit, base in zip(self.digits, yield_factorials(1)):
            num = self.digit_representation.from_digit(digit)
            if num is None:
                raise ValueError(f"{digit} could not be converted to an int.")
            if num > base:
                raise ValueError(f"{digit} (converted to {num}) is too large.")
            if num < 0:
                raise ValueError(f"{digit} (converted to {num}) is negative.")
            acc += base * digit
        return acc

    def __call__(self, items: list[T]) -> list[T]:
        if len(items) < 2:
            return items[:]
        if len(items) < len(self.permutation):
            return [items[i] for i in self.permutation[:len(items)]]
        return [items[i] for i in self.permutation] + items[len(self.permutation):]

    def __str__(self) -> str:
        return f"{self.digits[::-1]} ({int(self)})"

    def __add__(self, other: Union[int, 'FactorialInt']):
        if isinstance(other, int):
            return FactorialInt.from_int(int(self) + other)
        if not isinstance(other, FactorialInt):
            raise TypeError(f"Cannot add {other} to a FactorialInt. It must be an int or a FactorialInt instead of {type(other)}")
        return FactorialInt.from_int(int(self) + int(other))

    def __len__(self):
        return len(self.digits)


def permute_list(lst: list[T]) -> Iterable[list[T]]:
    if len(lst) < 2:
        yield lst
        return
    perm = FactorialInt.from_int(0)
    while len(perm) + 1 <= len(lst):
        yield perm(lst)
        perm = perm + 1


def ensure_prompt(prompt: str, validate: Callable[[str], Optional[str]]):
    response = input(prompt)
    while (error := validate(response)) is not None:
        print(error)
        response = input(prompt)
    return response


class CommandRegistry:
    registry: dict[str, Callable[[str, int, dict[int, 'FactorialInt[int]']], Optional[str]]]
    docs: dict[str, str]
    ctr: int
    state: dict[int, 'FactorialInt[int]']
    should_exit: bool

    def __init__(self) -> None:
        self.ctr = 0
        self.state = {}
        self.should_exit = False
        self.registry = {"q": lambda c, i, s: self.set_exit()}
        self.docs = {"q": "Quit"}

    def set_exit(self, should_exit: bool = True):
        self.should_exit = should_exit

    def __bool__(self):
        return not self.should_exit

    def __call__(self, prefix: str, doc: str):
        def wrapper(fn):
            self.registry[prefix] = fn
            self.docs[prefix] = doc
            return fn
        return wrapper

    def register_int_fn(self, prefix: str, doc: str):
        registerer = self(prefix, doc)
        def wrapper(fn):
            @functools.wraps(fn)
            def wrapped(response: str, ctr: int, state):
                if not response.isdigit():
                    return f"Expected numeric value, got '{response}' instead."
                return fn(int(response), ctr, state)
            return registerer(wrapped)
        return wrapper

    def register_state_reader(self, prefix: str, doc: str):
        registerer = self(prefix, doc)
        def wrapper(fn):
            @functools.wraps(fn)
            def wrapped(response: str, ctr: int, state):
                if not response.isdigit():
                    return f"Expected numeric value, got '{response}' instead."
                computed = state.get(int(response))
                if computed is None:
                    keys = ", ".join(map(str, state.keys()))
                    return f"Value {response} has not been computed, try one of {keys}"
                return fn(computed, ctr, state)
            return registerer(wrapped)
        return wrapper

    def handle_prompt(self, command: str) -> Optional[str]:
        if len(command) < 1:
            return "Please provide a command."
        fn, arg_str = command[0], command[1:]
        if fn not in self.registry:
            return f"Cannot handle function {fn}"
        return self.registry[fn](arg_str, self.ctr, self.state)

    def __str__(self) -> str:
        return ", ".join(f"{key} = {doc}" for key, doc in self.docs.items())

    def run_main(self):
        while self:
            print(str(self))
            ensure_prompt("Enter command > ", self.handle_prompt)
            self.ctr += 1


if __name__ == "__main__":
    main = CommandRegistry()

    @main.register_int_fn("!", "Convert int to factorial int")
    def convert_int(i: int, ctr: int, state):
        state[ctr] = FactorialInt.from_int(i)
        print(f"{i} is {state[ctr]} as a factorial int")

    @main.register_state_reader("S", "Get the permutation of the nth output")
    def get_perm(s: 'FactorialInt[int]', ctr: int, state):
        print(f"Perm({s}) = {s.permutation}")

    @main.register_int_fn("A", "Get all the permutations of n numbers")
    def all_perms(n: int, ctr: int, state):
        for i, perm in enumerate(permute_list(list(range(n)))):
            print(f"The {i}th permutation ({FactorialInt.from_int(i)}) is {perm}")

    main.run_main()
