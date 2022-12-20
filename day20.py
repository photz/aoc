import pytest
from typing import *


def read_input(path: str) -> List[int]:
    with open(path) as f:
        return list(map(int, f.read().strip().split("\n")))


T = TypeVar("T")


def mix(xs: list[T], i: int, move: int) -> list[T]:
    """
    Returns a new list in which xs[i] has been moved left or right by `move` steps.
    """

    if 0 == move:
        return xs

    x = xs[i]

    ys = xs[:i] + xs[i + 1 :]

    j = (i + move) % len(ys)

    return ys[:j] + [x] + ys[j:]


mix_tests = [
    ([1, 2, -3, 3, -2, 0, 4], 0, 1, [2, 1, -3, 3, -2, 0, 4]),
    ([2, 1, -3, 3, -2, 0, 4], 0, 2, [1, -3, 2, 3, -2, 0, 4]),
    ([1, -3, 2, 3, -2, 0, 4], 1, -3, [1, 2, 3, -2, -3, 0, 4]),
    ([1, 2, 3, -2, -3, 0, 4], 2, 3, [1, 2, -2, -3, 0, 3, 4]),
    ([1, 2, -2, -3, 0, 3, 4], 2, -2, [1, 2, -3, 0, 3, 4, -2]),
    ([1, 2, -3, 0, 3, 4, -2], 3, 0, [1, 2, -3, 0, 3, 4, -2]),
    ([1, 2, -3, 0, 3, 4, -2], 5, 4, [1, 2, -3, 4, 0, 3, -2]),
]


@pytest.mark.parametrize("_input,index,move,output", mix_tests)
def test_mix(_input, index, move, output):
    assert mix(_input, index, move) == output


def mix_all(numbers: List[int], iterations: int = 1) -> List[int]:

    state = list(enumerate(numbers))

    for _ in range(iterations):
        for i in range(len(state)):
            candidate_index = next(
                (
                    j
                    for j, (original_index, _) in enumerate(state)
                    if original_index == i
                )
            )

            original_index, move = state[candidate_index]

            state = mix(state, candidate_index, move)

    return [n for i, n in state]


def coords(mixed: Iterable[int]) -> int:

    zero_at = mixed.index(0)

    indexes = [1000, 2000, 3000]

    return sum((mixed[(zero_at + i) % len(mixed)] for i in indexes))


if __name__ == "__main__":
    numbers = read_input("./day20.txt")

    mixed = mix_all(numbers)

    print("Part 1:", coords(mixed))

    decryption_key = 811589153

    print(
        "Part 2:", coords(mix_all(list(map(lambda n: n * decryption_key, numbers)), 10))
    )
