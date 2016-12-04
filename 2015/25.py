def generate(seed):
    return (seed * 252533) % 33554393

def next_coord(row, col):
    if row == 1:
        return col + 1, 1
    else:
        return row - 1, col + 1

assert next_coord(1, 1) == (2, 1)
assert next_coord(2, 1) == (1, 2)
assert next_coord(1, 2) == (3, 1)

row, col = 1, 1
n = 20151125
while not (row == 2947 and col == 3029):
    n = generate(n)
    row, col = next_coord(row, col)

print n
