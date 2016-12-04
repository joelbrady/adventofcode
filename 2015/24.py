import sys

items = [int(n) for n in sys.stdin.readlines()]

total = sum(items)
assert total % 3 == 0
bin_weight = total / 3


