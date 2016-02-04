import sys
import re
import collections
import copy


class Wire(object):
    def __init__(self, output_name, val):
        self.v = None
        self.val = val
        self.output_name = output_name

    def value(self, wires=None):
        if self.v is None:
            try:
                self.v = int(self.val)
            except ValueError:
                self.v = wires[self.val].value(wires)
        return self.v

    def __repr__(self):
        return '{} -> {}'.format(self.val, self.output_name)


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
            try:
                x = int(self.x)
            except ValueError:
                x_wire = wires[self.x]
                x = x_wire.value(wires)
            try:
                y = int(self.y)
            except ValueError:
                y_wire = wires[self.y]
                y = y_wire.value(wires)
            self.v = x & y
        return self.v

    def __repr__(self):
        return '{} AND {} -> {}'.format(self.x, self.y, self.dst)


class OrWire(object):
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
            self.v = x | y
        return self.v

    def __repr__(self):
        return '{} OR {} -> {}'.format(self.x, self.y, self.dst)


class ShiftWire(object):
    def __init__(self, dst, src, scalar, shift_func, shift_symbol):
        self.dst = dst
        self.src = src
        self.scalar = scalar
        self.shift_func = shift_func
        self.shift_symbol = shift_symbol
        self.v = None
    
    def value(self, wires):
        if self.v is None:
            src = wires[self.src].value(wires)
            self.v = self.shift_func(src)
        return self.v

    def __repr__(self):
        return '{} {} {} -> {}'.format(self.src, self.shift_symbol, self.scalar, self.dst)


def main():
    with open('7.input.txt') as f:
        unevaluated = run(f.readlines())
        a = evaluate(copy.deepcopy(unevaluated))
        print a
        unevaluated['b'] = Wire('b', a)
        print evaluate(unevaluated)


def run(lines):
    unevaluated = {}
    for line in lines:
        process_line(unevaluated, line.strip())
    return unevaluated


def process_line(wires, line):
    if line == '':
        return
    for check in (val, noot, annd, orr, lshift, rshift):
        try:
            wire_dst, wire = check(line)
            wires[wire_dst] = wire
            return
        except TypeError:
            continue
    
    print
    print 'Could not process:', line
    print
    raise NotImplementedError


def val(s):
    def f(m, s):
        value, wire = m.group(1), m.group(2)
        return wire, Wire(wire, value)
    return match(r'\s*(\w+)\s*->\s*(\w+)', f, s)


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
        return dst, OrWire(dst, x, y)
    return match(r'(\w+)\s*OR\s*(\w+)\s*->\s*(\w+)', f, s)


def lshift(s):
    def f(m, s):
        x, y, dst = m.group(1), int(m.group(2)), m.group(3)
        def left_shift(v):
            result = v << y
            result %= 2 ** 16
            return result
        wire = ShiftWire(dst, x, y, left_shift, 'LSHIFT')
        return dst, wire
    return match(r'(\w+)\s*LSHIFT\s*(\d+)\s*->\s*(\w+)', f, s)


def rshift(s):
    def f(m, s):
        x, y, dst = m.group(1), int(m.group(2)), m.group(3)
        def right_shift(v):
            result = v >> y
            return result
        wire = ShiftWire(dst, x, y, right_shift, 'RSHIFT')
        return dst, wire
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
    a_val = evaluate(unevaluated_wires)
    assert a_val == 1


def simple_test():
    lines = [
        '123 -> x',
        '456 -> y',
        'x AND y -> d',
        'x OR y -> e',
        'x LSHIFT 2 -> f',
        'y RSHIFT 2 -> g',
        'NOT x -> a'
    ]
    unevaluated_wires = run(lines)
    a = evaluate(unevaluated_wires)
    assert a == 65412


if __name__ == '__main__':
    simple_test()
    test()
    main()


