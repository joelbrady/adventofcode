

with open('2.input') as f:
    instructions = f.readlines()


class Button(object):
    def __init__(self, value):
        self._value = value
        self._up = self
        self._down = self
        self._left = self
        self._right = self

    def value(self):
        return self._value

    def __repr__(self):
        return 'Button({})'.format(self.value())

    def up(self, neighbour=None):
        if neighbour is not None:
            self._up = neighbour
        return self._up

    def down(self, neighbour=None):
        if neighbour is not None:
            self._down = neighbour
        return self._down

    def left(self, neighbour=None):
        if neighbour is not None:
            self._left = neighbour
        return self._left

    def right(self, neighbour=None):
        if neighbour is not None:
            self._right = neighbour
        return self._right

one = Button('1')
two = Button('2')
three = Button('3')
four = Button('4')
five = Button('5')
six = Button('6')
seven = Button('7')
eight = Button('8')
nine = Button('9')
a = Button('A')
b = Button('B')
c = Button('C')
d = Button('D')

one.down(three)

two.right(three)
two.down(six)

three.up(one)
three.right(four)
three.down(seven)
three.left(two)

four.down(eight)
four.left(three)

five.right(six)

six.up(two)
six.right(seven)
six.down(a)
six.left(five)

seven.up(three)
seven.right(eight)
seven.down(b)
seven.left(six)

eight.up(four)
eight.right(nine)
eight.down(c)
eight.left(seven)

nine.left(eight)

a.up(six)
a.right(b)

b.up(seven)
b.right(c)
b.down(d)
b.left(a)

c.up(eight)
c.left(b)

d.up(b)

position = five

answer = ''

for instruction in instructions:
    instruction = instruction.strip()
    if len(instruction) == 0:
        continue
    for direction in instruction:
        if direction == 'U':
            position = position.up()
        elif direction == 'D':
            position = position.down()
        elif direction == 'L':
            position = position.left()
        elif direction == 'R':
            position = position.right()
        print position.value(),
    print 
    answer += position.value()

print answer
