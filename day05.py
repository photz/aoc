from typing import Tuple, Generator, Dict
import re
from dataclasses import dataclass

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

def split_at(xs: list['T'], index: int) -> Tuple[list['T'], list['T']]:
    """
    >>> split_at(['a', 'b'], 1)
    (['a'], ['b'])
    """
    return xs[:index], xs[index:]


def do_move(state: State, move: Move) -> State:

    src_stack = state[move.src]
    dest_stack = state[move.dest]

    if len(src_stack) < move.qty:
        raise Exception(f'Cannot move {move.qty} crates from stack {move.src}, because it has only {len(src_stack)} crates')

    src_stack_new, moving = split_at(src_stack, len(src_stack) - move.qty)

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

if __name__ == '__main__':
    moves = read('./day05.txt')

    state = initial

    for move in moves:
        state = do_move(state, move)

    print(tos(state))
