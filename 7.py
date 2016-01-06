import sys
import re

wires = {}
pending = []

class Wire(object):
    def __init__(self, value):
        self.value = value

    def value(self):
        return self.value


def main():
    for line in sys.stdin.readlines():
        process_line(line.strip())
    while len(pending) > 0:
        process_line(pending.pop())
    print wires


def process_line(line):
    for test in checks:
        if test(line):
            break
    for k in wires.keys():
        if wires[k] < 0:
            wires[k] += 2 **16


def val(s):
    def f(m, s):
        if m.group(1) == 'NOT':
            value, wire = m.group(2), m.group(3)
            value = ~int(value)
        else:
            value, wire = m.group(2), m.group(3)
        wires[wire] = int(value)
    return match(r'(NOT)?\s*(\d+)\s*->\s*(\w+)', f, s)


def noot(s):
    def f(m, s):
        src, dst = m.group(1), m.group(2)
        if src in wires.keys():
            wires[dst] = ~wires[src]
        else:
            pending.append(s)
    return match(r'NOT\s*(\w+)\s*->\s*(\w+)', f, s)


def annd(s):
    def f(m, s):
        x, y, dst = m.group(1), m.group(2), m.group(3)
        if x in wires.keys() and y in wires.keys():
            wires[dst] = wires[x] & wires[y]
        else:
            pending.append(s)
    return match(r'(\w+)\s*AND\s*(\w+)\s*->\s*(\w+)', f, s)


def orr(s):
    def f(m, s):
        x, y, dst = m.group(1), m.group(2), m.group(3)
        if x in wires.keys() and y in wires.keys():
            wires[dst] = wires[x] | wires[y]
        else:
            pending.append(s)
    return match(r'(\w+)\s*OR\s*(\w+)\s*->\s*(\w+)', f, s)


def lshift(s):
    def f(m, s):
        x, y, dst = m.group(1), int(m.group(2)), m.group(3)
        if x in wires.keys():
            wires[dst] = wires[x] << y
        else:
            pending.append(s)
    return match(r'(\w+)\s*LSHIFT\s*(\d+)\s*->\s*(\w+)', f, s)


def rshift(s):
    def f(m, s):
        x, y, dst = m.group(1), int(m.group(2)), m.group(3)
        if x in wires.keys():
            wires[dst] = wires[x] >> y
        else:
            pending.append(s)
    return match(r'(\w+)\s*RSHIFT\s*(\d+)\s*->\s*(\w+)', f, s)


def match(regex, f, s):
    m = re.match(regex, s)
    if m is not None:
        f(m, s)
        return True
    return False

checks = (val, noot, annd, orr, lshift, rshift)
if __name__ == '__main__':
    main()


