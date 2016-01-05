import itertools
import sys

d = set()
d.add((0, 0))

class Santa(object):
    def __init__(self):
        self.x = 0
        self.y = 0

    def move(self, c):
        if c == '^':
            self.x += 1
        elif c == 'v':
            self.x -= 1
        elif c == '<':
            self.y -= 1
        elif c == '>':
            self.y += 1

    def pos(self):
        return self.x, self.y

santa = Santa()
robosanta = Santa()
for c, s in zip(sys.stdin.read(), itertools.cycle((santa, robosanta))):
    s.move(c)
    d.add(s.pos())

print len(d)

