from typing import Tuple, Generator, Iterable

Range = Tuple[int, int]

def contains(a: Range, b: Range) -> bool:
    return a[0] <= b[0] and b[1] <= a[1]

def overlaps(a: Range, b: Range) -> bool:
    return (a[0] <= b[0] and b[0] <= a[1]) or \
        (b[0] <= a[0] and a[0] <= b[1])

def parse_range(s: str) -> Range:
    a, b = s.split('-')
    return int(a), int(b)

def read_input(path) -> Generator[Tuple[Range, Range], None, None]:
    with open(path) as f:
        for line in f.readlines():
            fst, snd = line.rstrip().split(',')
            yield parse_range(fst), parse_range(snd)

def problem1(pairs: Iterable[Tuple[Range, Range]]) -> int:
    return sum(
        1
        for a, b in pairs
        if contains(a, b) or contains(b, a)
    )

def problem2(pairs: Iterable[Tuple[Range, Range]]) -> int:
    return sum(
        1 for a, b in pairs if overlaps(a, b)
    )

if __name__ == '__main__':

    pairs = list(read_input('day04.txt'))

    print(problem1(pairs))
    print(problem2(pairs))



