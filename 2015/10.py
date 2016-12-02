import time

def look_and_say(n):
    n = str(n)
    result = ''
    while n != '':
        a = n[0]
        n = n[1:]
        count = 1
        while n != '' and n[0] == a:
            count += 1
            n = n[1:]
        result += str(count) + a
    return result

def main():
    s = '1321131112'
    for _ in range(50):
        s = look_and_say(s)
    print len(s)
main()
