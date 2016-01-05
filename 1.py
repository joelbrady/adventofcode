import sys

lines = sys.stdin.readlines()
s = ''.join(map(lambda a: a.strip(), lines))

floor = 0
position = 0
for c in s:
    position += 1
    if c == '(':
        floor += 1
    elif c == ')':
        floor -= 1
    else:
        assert False
    if floor == -1:
        print position
        sys.exit(0)

print floor
