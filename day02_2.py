from typing import Mapping, Iterable, Tuple, Generator
from enum import Enum

class Result(Enum):
    LOSS = 1
    DRAW = 2
    WIN = 3

class Shape(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

"""Mapping from characters to shapes"""
Shapemap = Mapping[str, Shape]

"""A Round is one move by "them" and one by "us"."""
Round = Tuple[Shape, Result]

beats: Mapping[Shape, Shape] = {
    Shape.ROCK: Shape.SCISSORS,
    Shape.SCISSORS: Shape.PAPER,
    Shape.PAPER: Shape.ROCK
}

succumbs_to: Mapping[Shape, Shape] = {
    loser: winner
    for winner, loser in beats.items()
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
    """
    >>> get_result(Shape.ROCK, Shape.ROCK)
    Result.DRAW
    """
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

def get_move(theirs: Shape, desired_outcome: Result) -> Shape:
    """
    Returns the move we need to make if the opponent uses `theirs`
    to get `desired_outcome`
    >>> get_move(Shape.ROCK, Result.WIN)
    Result.PAPER
    """
    match desired_outcome:
      case Result.WIN:
        return succumbs_to[theirs]

      case Result.LOSS:
        return beats[theirs]

      case Result.DRAW:
        return theirs


def rounds_from_file(path) -> Generator[Round, None, None]:
    char_to_shape: Shapemap = {
        'A': Shape.ROCK,
        'B': Shape.PAPER,
        'C': Shape.SCISSORS,
    }
    char_to_outcome: Mapping[str, Result] = {
        'X': Result.LOSS,
        'Y': Result.DRAW,
        'Z': Result.WIN,
    }
    with open(path) as f:
        for line in f.readlines():
            theirs_char, desired_outcome_char = line.rstrip().split(' ')
            theirs = char_to_shape[theirs_char]
            desired_outcome = char_to_outcome[desired_outcome_char]
            yield theirs, desired_outcome

def problem2(rounds: Iterable[Round]) -> int:
    return sum(
        get_score(theirs, get_move(theirs, desired_outcome))

        for theirs, desired_outcome in rounds
    )
    

if __name__ == '__main__':

    print(problem2(rounds_from_file('./day02.txt')))
