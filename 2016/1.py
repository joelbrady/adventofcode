import itertools
import sys

with open('1.input') as f:
    moves = ''.join(f.readlines()).strip().split(', ')

position = (0, 0)
direction = (0, 1)

def right(v):
    x, y = v
    return (y, -x)

def left(v):
    return right(right(right(v)))

def add(a, b):
    return (a[0] + b[0], a[1] + b[1])

for move in moves:
    if move[0] == 'L':
        direction = left(direction)
    else:
        assert move[0] == 'R'
        direction = right(direction)
    distance = int(move[1:])
    for _ in xrange(distance):
        position = add(position, direction)

print sum(position)
