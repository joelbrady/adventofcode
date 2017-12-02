
with open('1a.txt') as f:
    s = ''.join(f.readlines()).strip()
    total = 0
    for i in range(len(s)):
        next_index = (i + (len(s) // 2)) % len(s)
        if s[i] == s[next_index]:
            total += int(s[i])
    print(total)
