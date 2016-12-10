import sys
import re
import pdb


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

def ssl(ip):
    blocks = extract_blocks(ip)
    abas = get_abas(blocks)
    babs = set()
    for hypernet in extract_hypernets(ip):
        for s in substrings(hypernet):
            babs.add(s)
    for aba in abas:
        bab = aba_to_bab(aba)
        if bab in babs:
            return True
    return False

def aba_to_bab(aba):
    return aba[1] + aba[0] + aba[1]

def is_aba(aba):
    return aba[0] == aba[2] and aba[1] != aba[0]

assert is_aba('aba')
assert is_aba('zyz')
assert not is_aba('aaa')

assert aba_to_bab('aba') == 'bab'

def get_abas(blocks):
    abas = []
    for block in blocks:
        for aba in substrings(block):
            if is_aba(aba):
                abas.append(aba)
    return abas

def substrings(s):
    ss = set()
    for i in xrange(len(s) - 2):
        ss.add(s[i:i+3])
    return ss

assert substrings('aaaa') == set(['aaa'])
assert substrings('aaab') == set(['aaa', 'aab'])
assert substrings('abcde') == set(['abc', 'bcd', 'cde'])

assert get_abas(['abaa']) == ['aba']
assert get_abas(['aba', 'zyz']) == ['aba', 'zyz']

assert ssl('zazbz[bzb]cdb')
assert ssl('aba[bab]xyz')
assert not ssl('xyx[xyx]xyx')
assert ssl('aaa[kek]eke')

lines = sys.stdin.readlines()

print 'part a:', sum(1 for line in lines if verify(line))
print 'part b:', sum(1 for line in lines if ssl(line))
