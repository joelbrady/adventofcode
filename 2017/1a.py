
with open('1a.txt') as f:
    s = ''.join(f.readlines()).strip()
    total = 0
    for i in range(len(s)):
        if i == len(s) - 1:
            next_index = -1
        else:
            next_index = i + 1
        if s[i] == s[next_index]:
            total += int(s[i])
    print(total)
