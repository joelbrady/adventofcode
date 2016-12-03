

with open('2.input') as f:
    instructions = f.readlines()


# 1 2 3
# 4 5 6
# 7 8 9
# 5 is at 1, 1
# 1 is at 0, 0
# 7 is at 0, 2

def lookup(x, y):
    return x + y * 3 + 1

assert lookup(0, 0) == 1
assert lookup(1, 1) == 5
assert lookup(0, 2) == 7

x, y = 1, 1 # start at 5 in the centre

for instruction in instructions:
    instruction = instruction.strip()
    if len(instruction) == 0:
        continue
    for direction in instruction:
        if direction == 'U' and y > 0:
            y -= 1
        elif direction == 'D' and y < 2:
            y += 1
        elif direction == 'R' and x < 2:
            x += 1
        elif direction == 'L' and x > 0:
            x -= 1
    print lookup(x, y),
