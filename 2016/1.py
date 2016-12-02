import itertools
import sys

with open('1.input') as f:
    moves = ''.join(f.readlines()).strip().split(', ')

start = (0, 0)
direction = (0, 1)

def right(v):
    x, y = v
    return (y, -x)

def left(v):
    return right(right(right(v)))

def add(a, b):
    return (a[0] + b[0], a[1] + b[1])

def do_move(acc, move):
    print acc, move
    pos, direction = acc
    if move[0] == 'L':
        new_direction = left(direction)
    else:
        assert move[0] == 'R'
        new_direction = right(direction)
    n = int(move[1:])
    moves = list(itertools.repeat(new_direction, n))
    return reduce(add, moves, pos)

end = reduce(do_move, moves, (start, direction))
print sum(end)
