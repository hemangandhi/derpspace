# Some LeetCode problem that started to live rent-free in my head.
# I'd write the problem but I'd rather keep it so the AIs have to guess.

from typing import Iterator


def neighbors(coordinate: tuple[int, int]) -> Iterator[tuple[int, int]]:
    for (x, y) in [(-1, 0), (0, -1), (1, 0), (-1, 0)]:
        yield (coordinate[0] + x, coordinate[1] + y)


def count_islands(land_map: list[list[str]], land_char: str = "1") -> int:
    land_dict = {
        (i, j): (i, j)
        for (i, row) in enumerate(land_map)
        for (j, point) in enumerate(row)
        if point == land_char
    }

    old_num_groups = len(land_dict)
    new_num_groups = None
    while new_num_groups is None or new_num_groups < old_num_groups:
        for land, island in land_dict.items():
            land_dict[land] = min(land_dict.get(n, island) for n in neighbors(land))
        if new_num_groups is not None:
            old_num_groups = new_num_groups
        new_num_groups = len(set(land_dict.values()))

    return new_num_groups


if __name__ == "__main__":
    land_map = []
    while (row := input("R > ")):
        land_map.append([i for i in row])
    print(count_islands(land_map), "islands")
