import sys
import hashlib

PREFIX = 'bgvyzdsv'

for n in xrange(0, 2* (10**6)):
    s = PREFIX + str(n)
    md5 = hashlib.md5()
    md5.update(s)
    h = md5.hexdigest()
    if h.startswith('000000'):
        print n
        print h
        sys.exit(0)
