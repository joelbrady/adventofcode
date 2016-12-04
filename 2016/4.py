import re
import sys
from collections import Counter

def check(room):
    calculated_checksum = calculate_checksum(room)
    return calculated_checksum == get_checksum(room)

def calculate_checksum(room):
    frequency_tuples = letter_frequencies(get_room_name(room)).items()
    sorted_by_letter = sorted(frequency_tuples, key=lambda item: item[0])
    # python sorts are stable
    sorted_by_frequency = sorted(sorted_by_letter, key=lambda item: item[1], reverse=True)
    return ''.join(item[0] for item in sorted_by_frequency[:5])

def letter_frequencies(room_name):
    return Counter(room_name.replace('-', ''))

def get_room_name(room):
    return re.match('(.*)-\d+.*', room).group(1)

def get_checksum(room):
    return re.match('.*\[(.*)\]', room).group(1)

def get_room_number(room):
    return int(re.match('.*-(\d+).*', room).group(1))

def decrypt(room):
    room_name = get_room_name(room)
    room_number = get_room_number(room)
    return ''.join(map(lambda c: transliterate(c, room_number), room_name))

def transliterate(c, n):
    if ord('a') <= ord(c) <= ord('z'):
        offset = ord(c) - ord('a')
        return chr((offset + n) % 26 + ord('a'))
    elif c == '-':
        return ' '
    else:
        print c
        raise NotImplementedError

assert transliterate('a', 1) == 'b'
assert transliterate('z', 1) == 'a'
assert transliterate('a', 25) == 'z'
assert transliterate('a', 26) == 'a'
assert transliterate('a', 26 + 25) == 'z'

EXAMPLE_A = 'aaaaa-bbb-z-y-x-123[abxyz]'
assert get_room_name(EXAMPLE_A) == 'aaaaa-bbb-z-y-x'
assert get_room_number(EXAMPLE_A) == 123
assert get_checksum(EXAMPLE_A) == 'abxyz'
assert check(EXAMPLE_A)

EXAMPLE_B = 'a-b-c-d-e-f-g-h-987[abcde]'
assert get_room_name(EXAMPLE_B) == 'a-b-c-d-e-f-g-h'
assert get_checksum(EXAMPLE_B) == 'abcde'
assert get_room_number(EXAMPLE_B) == 987
assert check(EXAMPLE_B)

EXAMPLE_C = 'not-a-real-room-404[oarel]'
assert get_room_name(EXAMPLE_C) == 'not-a-real-room'
assert get_room_number(EXAMPLE_C) == 404
assert get_checksum(EXAMPLE_C) == 'oarel'
assert check(EXAMPLE_C)

assert not check('totally-real-room-200[decoy]')

rooms = [room.strip() for room in sys.stdin.readlines()]

print 'part a:', sum(get_room_number(room) for room in rooms if check(room))

valid_rooms = (room for room in rooms if check(room))

northpole_object_storage = filter(lambda room: decrypt(room) == 'northpole object storage', valid_rooms)[0]
print 'part b:', get_room_number(northpole_object_storage)

