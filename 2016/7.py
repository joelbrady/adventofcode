import sys
import re


def verify(ip):
    # at least one block needs an ABBA
    blocks = extract_blocks(ip)
    blocks_valid = any(has_abba(block) for block in blocks)

    # no hypernets can have an ABBA
    hypernets = extract_hypernets(ip)
    hypernets_valid = not any(has_abba(hypernet) for hypernet in hypernets)

    return blocks_valid and hypernets_valid

def has_abba(seq):
    for i in xrange(0, len(seq) - 4 + 1):
        subseq = seq[i:i+4]
        if is_abba(subseq):
            return True
    return False

def is_abba(candidate):
    assert len(candidate) == 4
    return candidate == candidate[::-1] and candidate[0] != candidate[1]


assert is_abba('abba')
assert not is_abba('aaaa')
assert has_abba('abbaba')
assert has_abba('abba')
assert not has_abba('aaaaaa')

def extract_blocks(ip):
    s = re.split('\[.*?\]', ip)
    return [t for t in s if len(t) > 0]

def extract_hypernets(ip):
    s = re.findall('\[.*?\]', ip)
    return [re.sub('\[|\]', '', t) for t in s]

EXAMPLE ='abc[def]ghi[jkl][mno]pqr'
a = extract_hypernets(EXAMPLE)
assert a == ['def', 'jkl', 'mno']
b = extract_blocks(EXAMPLE)
assert b == ['abc', 'ghi', 'pqr']

assert verify('abba[mnop]qrst')
assert not verify('abcd[bddb]xyyx')
assert not verify('aaaa[qwer]tyui')
assert verify('ioxxoj[asdfgh]zxcvbn')

lines = sys.stdin.readlines()
print 'part a:', sum(1 for line in lines if verify(line))
