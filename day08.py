import numpy.testing as npt
import pytest
import numpy as np
from typing import *
from functools import reduce


def read_grid(path):
    with open(path, "rb") as f:
        return np.array(
            [[c - ord("0") for c in line.rstrip()] for line in f.readlines()]
        )


@pytest.fixture(name="example")
def fixture_example():
    yield read_grid("./day08_example.txt")


@pytest.fixture(name="big_example")
def fixture_big_example():
    yield read_grid("./day08.txt")


def running_max_2d_lr(matrix):
    return np.apply_along_axis(np.maximum.accumulate, 1, matrix)


def test_running_max_2d(example):
    actual = running_max_2d_lr(example)
    expected = np.array(
        [
            [3, 3, 3, 7, 7],
            [2, 5, 5, 5, 5],
            [6, 6, 6, 6, 6],
            [3, 3, 5, 5, 9],
            [3, 5, 5, 9, 9],
        ]
    )
    npt.assert_array_equal(actual, expected)


def running_max_2d_rl(matrix):
    return np.fliplr(running_max_2d_lr(np.fliplr(matrix)))


def test_running_max_2d_rl(example):
    actual = running_max_2d_rl(example)
    expected = np.array(
        [
            [7, 7, 7, 7, 3],
            [5, 5, 5, 2, 2],
            [6, 5, 3, 3, 2],
            [9, 9, 9, 9, 9],
            [9, 9, 9, 9, 0],
        ]
    )
    npt.assert_array_equal(actual, expected)


def running_max_2d_tb(matrix):
    return np.rot90(running_max_2d_lr(np.rot90(matrix)), 3)


def test_running_max_2d_tb(example):
    actual = running_max_2d_tb(example)
    expected = np.array(
        [
            [3, 0, 3, 7, 3],
            [3, 5, 5, 7, 3],
            [6, 5, 5, 7, 3],
            [6, 5, 5, 7, 9],
            [6, 5, 5, 9, 9],
        ]
    )
    npt.assert_array_equal(actual, expected)


def running_max_2d_bt(matrix):
    return np.rot90(running_max_2d_lr(np.rot90(matrix, 3)))


def test_running_max_2d_bt(example):
    actual = running_max_2d_bt(example)
    expected = np.array(
        [
            [6, 5, 5, 9, 9],
            [6, 5, 5, 9, 9],
            [6, 5, 5, 9, 9],
            [3, 5, 5, 9, 9],
            [3, 5, 3, 9, 0],
        ]
    )
    npt.assert_array_equal(actual, expected)


def visible_from_left(col, row, grid, running_max) -> bool:
    return col == 0 or running_max[row, col - 1] < grid[row, col]


def visible_from_right(col, row, grid, running_max) -> bool:
    return col == len(grid) - 1 or running_max[row, col + 1] < grid[row, col]


def visible_from_top(col, row, grid, running_max) -> bool:
    return row == 0 or running_max[row - 1, col] < grid[row, col]


def visible_from_bottom(col, row, grid, running_max) -> bool:
    return row == len(grid) - 1 or running_max[row + 1, col] < grid[row, col]


def count_visible(grid) -> int:
    left_right = running_max_2d_lr(grid)
    right_left = running_max_2d_rl(grid)
    top_bottom = running_max_2d_tb(grid)
    bottom_top = running_max_2d_bt(grid)

    visible = (
        lambda col, row: visible_from_top(col, row, grid, top_bottom)
        or visible_from_bottom(col, row, grid, bottom_top)
        or visible_from_left(col, row, grid, left_right)
        or visible_from_right(col, row, grid, right_left)
    )

    return sum(
        (1 for row in range(len(grid)) for col in range(len(grid)) if visible(col, row))
    )


def test_count_visible(example):
    assert count_visible(example) == 21


def sightlines_lr(matrix):
    running_max = running_max_2d_lr(matrix)
    result = np.ones_like(matrix)

    for row in range(len(matrix)):
        result[row, 0] = 0

        for col in range(1, len(matrix)):

            if running_max[row, col - 1] < matrix[row, col]:
                result[row, col] = col

            elif matrix[row, col - 1] < matrix[row, col]:

                col0 = col - 1
                result[row, col] = 0
                # How to get rid of this loop?
                while 0 <= col0:
                    result[row, col] += 1
                    if matrix[row, col] <= matrix[row, col0]:
                        break
                    col0 -= 1

    return result


def test_sightlines_lr(example):
    actual = sightlines_lr(example)
    expected = np.array(
        [
            [0, 1, 2, 3, 1],
            [0, 1, 1, 1, 2],
            [0, 1, 1, 1, 1],
            [0, 1, 2, 1, 4],
            [0, 1, 1, 3, 1],
        ]
    )
    npt.assert_array_equal(actual, expected)


def test_sightlines_lr2():
    example = np.array(
        [
            [5, 1, 1, 5, 9],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]
    )
    actual = sightlines_lr(example)
    expected = np.array(
        [
            [0, 1, 1, 3, 4],
            [0, 1, 1, 1, 1],
            [0, 1, 1, 1, 1],
            [0, 1, 1, 1, 1],
            [0, 1, 1, 1, 1],
        ]
    )
    npt.assert_array_equal(actual, expected)


def sightlines_rl(matrix):
    return np.fliplr(sightlines_lr(np.fliplr(matrix)))


def test_sightlines_rl(example):
    actual = sightlines_rl(example)
    expected = np.array(
        [
            [2, 1, 1, 1, 0],
            [1, 1, 2, 1, 0],
            [4, 3, 1, 1, 0],
            [1, 1, 2, 1, 0],
            [1, 2, 1, 1, 0],
        ]
    )
    npt.assert_array_equal(
        actual,
        expected,
    )


def sightlines_tb(matrix):
    return np.rot90(sightlines_lr(np.rot90(matrix)), 3)


def sightlines_bt(matrix):
    return np.rot90(sightlines_lr(np.rot90(matrix, 3)))


def highest_scenic_score(grid) -> int:
    matrices = [
        sightlines_lr(grid),
        sightlines_rl(grid),
        sightlines_tb(grid),
        sightlines_bt(grid),
    ]
    return np.max(reduce(np.multiply, matrices))


def test_highest_scenic_score(example):
    assert highest_scenic_score(example) == 8


def test_highest_scenic_score_big_example(big_example):
    assert highest_scenic_score(big_example) == 287040


if __name__ == "__main__":
    grid = read_grid("./day08.txt")

    print(count_visible(grid))
    print(highest_scenic_score(grid))
