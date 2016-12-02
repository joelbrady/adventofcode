import sys
import re

def simplify_once(s):
    # quotes
    m = re.search(r'\"', s)
    if m is not None:
        return s.replace(r'\"', '"')
    m = re.search(r'\\x(\d\d)', s)
    if m is not None:
        ascii_code = int(m.group(1), 16)
        return s.replace(m.group(0), chr(ascii_code))
    return s.replace(r'\\\\', r'\\')

def simplify(s):
    prev = s
    s = simplify_once(s)
    while s != prev:
        s = simplify_once(s)
    return s

print simplify('\\x44\\"')
