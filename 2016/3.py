

with open('3.input') as f:
    triangles = f.readlines()

n = 0

for triangle in triangles:
    triangle = triangle.strip()
    a, b, c = sorted(map(int, triangle.split()))
    if a + b > c:
        n += 1

print 'part a', n

n = 0

def isTriangle(t):
    t = sorted(t)
    return t[0] + t[1] > t[2]

assert len(triangles) % 3 == 0
for i in xrange(len(triangles) / 3):
    a, b, c = triangles[i * 3], triangles[i * 3 + 1], triangles[i * 3 + 2]
    a = a.split()
    b = b.split()
    c = c.split()
    one = [a[0], b[0], c[0]]
    two = [a[1], b[1], c[1]]
    three = [a[2], b[2], c[2]]
    one = map(int, one)
    two = map(int, two)
    three = map(int, three)
    if isTriangle(one):
        n += 1
    if isTriangle(two):
        n += 1
    if isTriangle(three):
        n += 1

print 'part b', n
