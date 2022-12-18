import os
from suffix_tree import Tree
from datetime import datetime
import pytest
from typing import *
from itertools import cycle, islice
from dataclasses import dataclass
from bitstring import BitArray


class Map(object):
    def __init__(self, width: int, preserve_rows: int):
        self.width = width
        self.preserve_rows = preserve_rows
        self.array = BitArray(width * preserve_rows)
        self.current_offset = 0

    def _idx(self, x: int, y: int) -> int:
        if self.current_offset + self.preserve_rows <= y:
            self.set_offset(y - self.preserve_rows + 1)

        return x + self.width * (y - self.current_offset)

    def set(self, x: int, y: int):
        idx = self._idx(x, y)
        self.array[idx] = 1

    def is_set(self, x: int, y: int) -> bool:
        idx = self._idx(x, y)
        assert 0 <= idx
        return self.array[idx]

    def drop_row(self):
        self.array >> self.width
        self.current_offset += 1

    def set_offset(self, new_offset: int):
        assert self.current_offset <= new_offset

        self.array <<= self.width * (new_offset - self.current_offset)
        self.current_offset = new_offset


def test_map_simple():
    m = Map(10, 3)
    m.set(0, 0)
    assert m.is_set(0, 0)
    m.set(0, 1)
    assert m.is_set(0, 1)
    m.set(0, 2)
    assert m.is_set(0, 2)

    m.set(9, 0)
    assert m.is_set(9, 0)

    m.set(0, 3)
    assert m.is_set(0, 3)
    with pytest.raises(Exception):
        assert m.is_set(0, 0)


def test_map_bumping():
    m = Map(5, 10)
    m.set(3, 0)
    assert m.is_set(3, 0)

    m.set(4, 11)
    assert m.is_set(4, 11)
    assert not m.is_set(2, 11)

    with pytest.raises(Exception):
        assert m.is_set(3, 0)

    m.set(4, 30)
    assert m.is_set(4, 30)
    assert not m.is_set(3, 30)

    with pytest.raises(Exception):
        assert m.is_set(1, 13)


@dataclass
class Shape:
    width: int
    height: int
    shape: Tuple[int, ...]

    def points(self):
        for i, v in enumerate(self.shape):

            x = i % self.width
            y = i // self.width

            if v == 1:
                yield x, y


shapes = {
    "-": Shape(4, 1, (1, 1, 1, 1)),
    "+": Shape(
        3,
        3,
        (
            0,
            1,
            0,
            1,
            1,
            1,
            0,
            1,
            0,
        ),
    ),
    "L": Shape(
        3,
        3,
        (
            0,
            0,
            1,
            0,
            0,
            1,
            1,
            1,
            1,
        ),
    ),
    "|": Shape(
        1,
        4,
        (1, 1, 1, 1),
    ),
    "cube": Shape(2, 2, (1, 1, 1, 1)),
}


def falling_shapes() -> Generator[Shape, None, None]:
    yield from cycle(
        [shapes["-"], shapes["+"], shapes["L"], shapes["|"], shapes["cube"]]
    )


def read_input(path: str) -> str:
    with open(path) as f:
        return f.read().strip()


def draw(cave_width, solidified, inflight: Tuple[int, int, Shape], draw_whole=False):
    inflight_x, inflight_y, shape = inflight

    def is_inflight(x: int, y: int) -> bool:

        if y < inflight_y - shape.height:
            return False

        if inflight_y < y:
            return False

        if x < inflight_x:
            return False

        if inflight_x + shape.width < x:
            return False

        return (x, y) in (
            (px + inflight_x, inflight_y - py) for px, py in shape.points()
        )

    def is_set(x, y):
        try:
            return solidified.is_set(x, y)
        except Exception as e:
            return False

    def char_at(x: int, y: int) -> str:
        if y == -1 and (x < 0 or cave_width <= x):
            return "+"
        elif y == -1:
            return "-"
        elif x < 0 or cave_width <= x:
            return "|"
        elif is_set(x, y):
            return "#"
        elif is_inflight(x, y):
            return "@"
        else:
            return "."

    if draw_whole:
        until_y = -2
    else:
        until_y = max(inflight_y - 40, -2)

    for y in range(inflight_y + 3, until_y, -1):
        print("".join((char_at(x, y) for x in range(-1, cave_width + 1))))


def points(shape_x, shape_y, shape) -> Generator[Tuple[int, int], None, None]:
    for x, y in shape.points():
        yield x + shape_x, shape_y - y


def collides(shape_x, shape_y, shape, solidified) -> bool:
    for px, py in points(shape_x, shape_y, shape):
        if solidified.is_set(px, py):
            return True

    return False


def on_ground(shape_y, shape) -> bool:
    return 0 == shape_y - shape.height + 1


def can_move_left(shape_x, shape_y, shape, solidified):
    return 0 < shape_x and not collides(shape_x - 1, shape_y, shape, solidified)


def can_move_right(shape_x, shape_y, shape, solidified):
    cave_width = 7
    return shape_x < cave_width - shape.width and not collides(
        shape_x + 1, shape_y, shape, solidified
    )


def can_fall_further(shape_x, shape_y, shape, solidified):
    return not on_ground(shape_y, shape) and not collides(
        shape_x, shape_y - 1, shape, solidified
    )


def simulate(cave_width: int, spawn_x: int, margin_y: int, shapes, gusts):

    solidified = Map(cave_width, 50)

    current_h = -1

    # rows = []

    # states = {}

    # f = open('pattern-example-with-fallen-shapes.txt', 'w')

    for i, shape in enumerate(islice(falling_shapes(), 2022)):

        shape_x = spawn_x
        shape_y = current_h + margin_y + shape.height

        while True:
            direction = next(gusts)

            # input('========= ' + direction + ' ========== ' + str(current_h))
            # print(chr(27) + "[2J")
            # draw(cave_width, solidified, (shape_x, shape_y, shape))

            if direction == "<" and can_move_left(shape_x, shape_y, shape, solidified):
                shape_x -= 1
            elif direction == ">" and can_move_right(
                shape_x, shape_y, shape, solidified
            ):
                shape_x += 1

            if not can_fall_further(shape_x, shape_y, shape, solidified):
                break

            shape_y -= 1

        # if solidified.array.b in states:
        #     print('seen this before', (i, current_h), states[solidified.array.b])
        #     prev_i, prev_current_h = states[solidified.array.b]

        #     pattern_length = current_h - prev_current_h
        #     print('pattern length', pattern_length)

        #     fallen_shapes_before_pattern_start = prev_i
        #     print('fallen shapes before pattern start', prev_i)

        #     return

        # else:
        #     states[solidified.array.b] = (i, current_h)

        for px, py in points(shape_x, shape_y, shape):
            solidified.set(px, py)

        if current_h < shape_y:
            pass

            # for row in range(max(current_h, 0), shape_y):
            #     new_row = ''.join((
            #         'A' if solidified.is_set(x, row) else 'B' for x in range(0, cave_width)
            #     ))

            #     f.write(new_row + ' ' + str(i) + '\n')
            #     rows.append(new_row)

        current_h = max(shape_y, current_h)

    # with open('pattern-example.txt', 'w') as f:
    #     f.write('\n'.join(rows))

    return current_h + 1


if __name__ == "__main__":
    jetpattern = read_input("./day17.txt")

    print(simulate(7, 2, 3, falling_shapes(), cycle(jetpattern)))
