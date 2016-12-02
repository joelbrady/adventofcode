import sys

def f(s):
    l,w,h=map(int,s.split('x'))
    a = 2 * l + 2 * h
    b = 2 * l + 2 * w
    c = 2 * w + 2 * h
    return min(a,b,c) + l*w*h

print sum(f(s) for s in sys.stdin.readlines())

