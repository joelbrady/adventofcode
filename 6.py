import sys
import re

grid = {}
for x in xrange(1000):
    for y in xrange(1000):
        grid[(x, y)] = 0

def turn_on(coords):
    grid[coords] += 1

def turn_off(coords):
    grid[coords] = max(0, grid[coords] - 1)

def toggle(coords):
    grid[coords] += 2

def process_line(line):
    line = line.strip()
    if line.startswith('turn on'):
        do(turn_on, line[len('turn on '):])
    elif line.startswith('turn off'):
        do(turn_off, line[len('turn off '):])
    elif line.startswith('toggle'):
        do(toggle, line[len('toggle '):])

def do(f, line):
    start, end = get_endpoints(line)
    start_x, start_y = start
    end_x, end_y = end
    assert start_x <= end_x
    assert start_y <= end_y
    for x in xrange(start_x, end_x + 1):
        for y in xrange(start_y, end_y + 1):
            f((x, y))

def get_endpoints(line):
    groups = re.search(r'(\d+),\s*(\d+)\s*through\s*(\d+),\s*(\d+)', line)
    if groups is None:
        print line
        sys.exit(1)
    start_x = int(groups.group(1))
    start_y = int(groups.group(2))
    end_x = int(groups.group(3))
    end_y = int(groups.group(4))
    return ((start_x, start_y), (end_x, end_y))

def main():
    for line in sys.stdin.readlines():
        process_line(line)
    total = 0
    for x in xrange(1000):
        for y in xrange(1000):
            total += grid[(x, y)]
    print total

if __name__ == '__main__':
    main()

