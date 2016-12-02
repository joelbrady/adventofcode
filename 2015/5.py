import sys, re

s = sys.stdin.read().strip()
if s is not None and s != '' and re.search(r'([a-z])\1', s):
    print s
