from itertools import count
from typing import Tuple, Iterable, Generator, NewType
from sys import argv

example_rucksacks = [
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw",
]

def priority(item_type: str) -> int:
    """
    >>> priority("a")
    1
    >>> priority("z")
    26
    >>> priority("A")
    27
    """
    code = ord(item_type)
    if ord('a') <= code and code <= ord('z'):
        return code - ord('a') + 1
    elif ord('A') <= code and code <= ord('Z'):
        return code - ord('A') + 27
    else:
        raise Exception(f'{item_type} is not a valid item type')

def even(n: int) -> bool:
    """
    >>> even(2)
    True
    """
    return n % 2 == 0

def compartments(rucksack: str) -> Tuple[str, str]:
    if not even(len(rucksack)):
        raise Exception(f'rucksack cannot have uneven length')
    cutoff = len(rucksack) // 2 
    return rucksack[:cutoff], rucksack[cutoff:]

def problem1(rucksacks: Iterable[str]) -> int:
    return sum(
        priority(next(iter(set(first_comp).intersection(set(second_comp)))))
        
        for first_comp, second_comp in map(compartments, rucksacks)
    )

def groups(xs: Iterable['T'], size: int) -> Generator['T', None, None]:
    """
    >>> list(groups([1,2,3,4], 2))
    [(1, 2), (3, 4)]
    """

    yield from (
        tuple(xs[offset:offset + size])
        for offset in range(0, len(xs), size)
    )

def problem2(rucksacks: Iterable[str]) -> int:
    return sum(
        priority(next(iter(set(a).intersection(set(b)).intersection(set(c)))))
        for a, b, c in groups(rucksacks, 3)
    )

if __name__ == '__main__':

    with open(argv[1]) as f:
        rucksacks = list(line.rstrip() for line in f.readlines())

    print(problem1(rucksacks))

    print(problem2(rucksacks))
