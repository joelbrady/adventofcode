import sys
from collections import Counter, defaultdict

columns = defaultdict(str)
for line in sys.stdin.readlines():
    i = 0
    for c in line.strip():
        columns[i] += c
        i += 1

answer = ''
part_b = ''
for col in xrange(len(columns)):
    c = Counter(columns[col])
    a = c.most_common(1)
    answer += a[0][0]

    b = sorted(c.items(), key=lambda t: t[1])
    part_b += b[0][0]
print 'part a:', answer
print 'part b:', part_b
