import sys

m = {}
towns = set()

def add_path(src, dst, distance):
    distance = int(distance)
    m[(src, dst)] = distance
    m[(dst, src)] = distance
    towns.add(src)
    towns.add(dst)

def get_distance(src, dst):
    if (src, dst) in m:
        return m[(src, dst)]
    return None

with open('8.input.txt') as f:
    for line in f.readlines():
        line = line.strip()
        src, _, dst, _, distance = line.split()
        add_path(src, dst, distance)

paths = []

def dfs(current_path):
    if len(current_path) == len(towns):
        paths.append(current_path)
    current_location = current_path[-1]
    for town in towns:
        if town in current_path:
            continue
        distance_to_town = get_distance(current_location, town)
        if distance_to_town is None:
            continue
        dfs(current_path + [town])

for town in towns:
    dfs([town])

def length(path):
    total = 0
    for src, dst in zip(path, path[1:]):
        total += get_distance(src, dst)
    return total

shortest = 0
shortest_path = None

for path in paths:
    l = length(path)
    if l > shortest:
        shortest = l
        shortest_path = path

print shortest
print shortest_path
