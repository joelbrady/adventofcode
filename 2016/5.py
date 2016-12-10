import hashlib

def md5(s):
    m = hashlib.md5()
    m.update(s)
    return m.hexdigest()

s = 'wtnhxymk'

i = 0
answer = ['_'] * 8
found = 0
while found < 8:
    h = md5(s + str(i))
    if h[:5] == '00000':
        try:
            idx = int(h[5])
            if idx > 7:
                i += 1
                continue
        except ValueError:
            i += 1
            continue
        if answer[idx] == '_':
            answer[idx] = h[6]
            found += 1
            print ''.join(answer)
    i += 1

