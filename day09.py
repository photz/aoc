from itertools import islice
from enum import Enum
from typing import Generator, Tuple, Iterable


class Dir(Enum):
    Up = 1
    Down = 2
    Left = 3
    Right = 4


char_to_dir = {
    "U": Dir.Up,
    "D": Dir.Down,
    "L": Dir.Left,
    "R": Dir.Right,
}

Pos = Tuple[int, int]

Motion = Tuple[Dir, int]


def read_input(path) -> Generator[Motion, None, None]:
    with open(path) as f:
        for line in f.readlines():
            direction, steps = line.split(" ")
            yield char_to_dir[direction], int(steps)


def apply_motion(pos: Pos, motion: Motion) -> Generator[Pos, None, None]:
    x, y = pos

    match motion[0]:
        case Dir.Up:
            yield from ((x, y - i) for i in range(1, motion[1] + 1))
        case Dir.Down:
            yield from ((x, y + i) for i in range(1, motion[1] + 1))
        case Dir.Left:
            yield from ((x - i, y) for i in range(1, motion[1] + 1))
        case Dir.Right:
            yield from ((x + i, y) for i in range(1, motion[1] + 1))


def get_positions(
    initial: Pos, motions: Iterable[Motion]
) -> Generator[Pos, None, None]:
    current = initial

    for motion in motions:

        for pos in apply_motion(current, motion):

            current = pos
            yield current


def touching(head: Pos, tail: Pos) -> bool:
    delta_x = head[0] - tail[0]
    delta_y = head[1] - tail[1]
    return abs(delta_x) <= 1 and abs(delta_y) <= 1


def sign(n: int) -> int:
    if 0 <= n:
        return 1
    else:
        return -1


def trail(head: Pos, tail: Pos) -> Pos:
    if touching(head, tail):
        return tail

    delta_x = head[0] - tail[0]
    delta_y = head[1] - tail[1]

    if delta_y == 0:
        return tail[0] + sign(delta_x), tail[1]

    elif delta_x == 0:
        return tail[0], tail[1] + sign(delta_y)

    return tail[0] + sign(delta_x), tail[1] + sign(delta_y)


def test_trail():
    head = 3, 1
    tail = 1, 1
    assert trail(head, tail) == (2, 1)


def test_trail2():
    head = 1, 3
    tail = 1, 1
    assert trail(head, tail) == (1, 2)


def test_trail3():
    head = 2, 1
    tail = 1, 3
    assert trail(head, tail) == (2, 2)


def test_trail4():
    head = 3, 2
    tail = 1, 3
    assert trail(head, tail) == (2, 2)


def positions_visited(motions: Iterable[Motion], n_knots: int) -> int:
    visited = set()

    initial = 0, 0

    knots = [initial] * n_knots

    for head in get_positions(initial, motions):

        for i, knot in enumerate(knots):

            tracked = head if i == 0 else knots[i - 1]

            knots[i] = trail(tracked, knot)

        visited.add(knots[-1])

    return len(visited)


def test_part2():
    initial = 0, 0
    motions = [
        (Dir.Right, 5),
        (Dir.Up, 8),
        (Dir.Left, 8),
        (Dir.Down, 3),
        (Dir.Right, 17),
        (Dir.Down, 10),
        (Dir.Left, 25),
        (Dir.Up, 20),
    ]
    assert positions_visited(motions, 9) == 36


if __name__ == "__main__":
    motions = list(read_input("./day09.txt"))

    print(positions_visited(motions, 1))
    print(positions_visited(motions, 9))
