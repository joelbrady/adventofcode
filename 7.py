import sys
import re

wires = {}
def main():
    for line in sys.stdin.readlines():
        line = line.strip()

        for test in checks:
            test(line)
    print wires

def val(s):
    m = re.match(r'(NOT)?\s*(\d+)\s*->\s*(\w+)', s)
    if m is not None:
        if m.group(1) == 'NOT':
            value, wire = m.group(2), m.group(3)
            value = ~int(value)
        else:
            value, wire = m.group(1), m.group(2)
        print dir(m.groups)
        wires[wire] = int(value)

checks = (val,)
if __name__ == '__main__':
    main()


