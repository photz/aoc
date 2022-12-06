from typing import Tuple, Generator, Dict, Iterable, TypeVar
import re
from dataclasses import dataclass
from functools import reduce, partial

@dataclass
class Move:
    qty: int
    src: int
    dest: int

State = Dict[int, list[str]]

initial: State = {
    1: ['R', 'P', 'N', 'R'],
    2: ['T', 'J', 'B', 'L', 'C', 'S', 'V', 'H'],
    3: ['T', 'D', 'B', 'M', 'N', 'L'],
    4: ['R', 'V', 'P', 'S', 'B'],
    5: ['G', 'C', 'Q', 'S', 'W', 'M', 'V', 'H'],
    6: ['W', 'Q', 'S', 'C', 'D', 'B', 'J'],
    7: ['F', 'Q', 'L'],
    8: ['W', 'M', 'H', 'T', 'D', 'L', 'F', 'V'],
    9: ['L', 'P', 'B', 'V', 'M', 'J', 'F'],
}

def read(path) -> Generator[Move, None, None]:
    with open(path) as f:
        lines = f.readlines()

    for line in lines:
        m = re.match('move (?P<qty>\d+) from (?P<src>\d+) to (?P<dest>\d+)', line)
        
        if m:
            groups = m.groupdict()
            
            yield Move(
                qty = int(groups['qty']),
                src = int(groups['src']),
                dest = int(groups['dest']))

T = TypeVar('T')
def split_at(xs: list[T], index: int) -> Tuple[list[T], list[T]]:
    """
    >>> split_at(['a', 'b'], 1)
    (['a'], ['b'])
    """
    return xs[:index], xs[index:]


def do_move(state: State, move: Move, reverse_order: bool = True) -> State:

    src_stack = state[move.src]
    dest_stack = state[move.dest]

    if len(src_stack) < move.qty:
        raise Exception(f'Cannot move {move.qty} crates from stack {move.src}, because it has only {len(src_stack)} crates')

    src_stack_new, moving = split_at(src_stack, len(src_stack) - move.qty)

    if reverse_order:
        moving.reverse()

    dest_stack_new = [
        *dest_stack,
        *moving,
    ]

    return {
        **state,
        move.src: src_stack_new,
        move.dest: dest_stack_new,
    }

def tos(state: State) -> str:
    return ''.join(
        stack[-1]
        for stack in state.values()
    )

def do_moves(initial: State, moves: Iterable[Move], reverse_order: bool = True) -> State:
    return reduce(partial(do_move, reverse_order=reverse_order), moves, initial)

if __name__ == '__main__':
    moves = list(read('./day05.txt'))

    print('Problem 1: %s' % tos(do_moves(initial, moves)))

    print('Problem 2: %s' % tos(do_moves(initial, moves, reverse_order=False)))
