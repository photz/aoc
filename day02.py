from typing import Mapping, Iterable, Tuple, Generator
from enum import Enum

example_rounds = [
    'A Y',
    'B X',
    'C Z',
]

class Result(Enum):
    LOSS = 1
    DRAW = 2
    WIN = 3

class Shape(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

Shapemap = Mapping[str, Shape]

beats: Mapping[Shape, Shape] = {
    Shape.ROCK: Shape.SCISSORS,
    Shape.SCISSORS: Shape.PAPER,
    Shape.PAPER: Shape.ROCK
}

result_score: Mapping[Result, int] = {
    Result.WIN: 6,
    Result.DRAW: 3,
    Result.LOSS: 0,
}

shape_score: Mapping[Shape, int] = {
    Shape.ROCK: 1,
    Shape.PAPER: 2,
    Shape.SCISSORS: 3,
}

def get_result(theirs: Shape, ours: Shape) -> Result:
    if theirs == ours:
        return Result.DRAW
    elif beats[ours] == theirs:
        return Result.WIN
    else:
        return Result.LOSS

def get_score(theirs: Shape, ours: Shape) -> int:
    """
    >>> get_score(Shape.ROCK, Shape.PAPER)
    8
    """
    return shape_score[ours] + result_score[get_result(theirs, ours)]

def problem1(rounds: Iterable[Tuple[Shape, Shape]]) -> int:
    return sum(
        get_score(theirs, ours)
        for theirs, ours in rounds
    )

def rounds_from_file(path, shapemap: Shapemap) -> Generator[Tuple[Shape, Shape], None, None]:
    with open(path) as f:
        for line in f.readlines():
            theirs_char, ours_char = line.rstrip().split(' ')
            theirs, ours = shapemap[theirs_char], shapemap[ours_char]
            yield (theirs, ours)

if __name__ == '__main__':
    char_to_shape: Shapemap = {
        'A': Shape.ROCK,
        'B': Shape.PAPER,
        'C': Shape.SCISSORS,
        'Y': Shape.PAPER,
        'X': Shape.ROCK,
        'Z': Shape.SCISSORS,
    }    
    print('Problem 1:', problem1(rounds_from_file('./day02.txt', char_to_shape)))
    
