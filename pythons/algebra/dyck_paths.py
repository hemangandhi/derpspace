# From https://oeis.org/A080936


def binomial_coeff(b: int, d: int) -> int:
    if d == 0 or d == b:
        return 1
    return binomial_coeff(b - 1, d - 1) + binomial_coeff(b - 1, d)


def powers_of_minus1(starting_power: int = 0):
    sign = 1 if starting_power % 2 == 0 else -1
    while True:
        yield sign
        sign = -sign


def dyck_paths(length: int, depth: int) -> int:
    coefficients = [
        sum(
            p * binomial_coeff(i + 2 * depth - 2 * k + 1, i)
            for i, p in zip(range(k + 1), powers_of_minus1(k))
        )
        for k in range(1, depth + 1)
    ]

    nums = [0, 1]
    for j in range(1, length + 1):
        nums.append(
            sum(
                p * nums[max(0, j - (i - 1))] * coefficients[i - 1]
                for i, p in zip(
                    range(1, min(length - (depth - 1), depth + 1)), powers_of_minus1()
                )
            )
        )
    return nums[length - (depth - 1)]


def print_dyck_tree(max_length: int):
    for row in range(1, max_length + 1):
        print([dyck_paths(row, col) for col in range(1, row + 1)])
