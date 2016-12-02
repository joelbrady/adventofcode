import sys
import re

count = 0
for s in sys.stdin.readlines():
    s = s.strip()
    a = re.search(r'([a-z]{2}).*\1', s)
    b = re.search(r'([a-z])[a-z]\1', s)
    if a is not None and b is not None:
        count += 1

print count
