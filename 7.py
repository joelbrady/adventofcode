import sys
import re
import collections


class Wire(object):
    def __init__(self, output_name, value):
        self.v = int(value)
        self.output_name = output_name

    def value(self, wires=None):
        return self.v

    def __repr__(self):
        return '{} -> {}'.format(self.v, self.output_name)


class NotWire(object):
    def __init__(self, dst, src):
        self.dst = dst
        self.src = src
        self.v = None

    def value(self, wires):
        if self.v is None:
            src_val = ~wires[self.src].value(wires)
            if src_val > 0:
                self.v = src_val
            else:
                self.v = src_val + 2**16
        return self.v

    def __repr__(self):
        return 'NOT {} -> {}'.format(self.src, self.dst)


class AndWire(object):
    def __init__(self, dst, x, y):
        self.dst = dst
        self.x = x
        self.y = y
        self.v = None

    def value(self, wires):
        if self.v is None:
            x_wire = wires[self.x]
            y_wire = wires[self.y]
            x = x_wire.value(wires)
            y = y_wire.value(wires)
            self.v = x & y
        return self.v

    def __repr__(self):
        return '{} AND {} -> {}'.format(self.x, self.y, self.dst)

def main():
    print run(sys.stdin.readlines())


def run(lines):
    unevaluated = {}
    for line in lines:
        process_line(unevaluated, line.strip())
    return unevaluated


def process_line(wires, line):
    for check in (val, noot, annd):
        try:
            wire_dst, wire = check(line)
            wires[wire_dst] = wire
        except TypeError:
            continue


def val(s):
    def f(m, s):
        if m.group(1) == 'NOT':
            value, wire = m.group(2), m.group(3)
            value = ~int(value)
            raise NotImplementedError
        else:
            value, wire = m.group(2), m.group(3)
            return wire, Wire(wire, value)
    return match(r'(NOT)?\s*(\d+)\s*->\s*(\w+)', f, s)


def noot(s):
    def f(m, s):
        src, dst = m.group(1), m.group(2)
        return dst, NotWire(dst, src)
    return match(r'NOT\s*(\w+)\s*->\s*(\w+)', f, s)


def annd(s):
    def f(m, s):
        x, y, dst = m.group(1), m.group(2), m.group(3)
        return dst, AndWire(dst, x, y)
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
        return f(m, s)
    return None


def evaluate(wires):
    a = wires['a']
    return a.value(wires)


def test():
    with open('7.example.input') as test_input:
        lines = test_input.readlines()
    unevaluated_wires = run(lines)
    wires = evaluate(unevaluated_wires)
    assert wires == {
        'd': 72,
        'e': 507,
        'f': 492,
        'g': 114,
        'h': 65412,
        'i': 65079,
        'x': 123,
        'y': 456,
        'a': 1
    }


def simple_test():
    lines = [
        '123 -> x',
        '456 -> y',
        'x AND y -> a'
    ]
    unevaluated_wires = run(lines)
    print unevaluated_wires
    a = evaluate(unevaluated_wires)
    print a
    assert a == 72 


if __name__ == '__main__':
    simple_test()
    test()
    main()


