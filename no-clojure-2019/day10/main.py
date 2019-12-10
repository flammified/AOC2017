import networkx
import parse
import re
import math
import collections
import functools


output = None


input = open('input.txt', 'r').read().splitlines()
test = open('test.txt', 'r').read().splitlines()

def split(word):
    return [char for char in word]

points = []
for y, l in enumerate(input):
    for x, char in enumerate(split(l)):
        if (char == "#"):
            points.append((x, y))

maxp = 0
bestp = None
bestangles = {}

def dist(p2, p):
    x,y = p
    x2,y2 = p2
    return math.sqrt((x - x2)**2 + (y - y2)**2)

angles = None
for pos in points:

    posX, posY = pos
    angles = collections.defaultdict(list)
    res = 0
    for point in points:
        if point == (posX, posY):
            continue
        x,y = point
        angle = math.degrees(math.atan2(-1 * (y - posY), x-posX)) - 90
        if angle < 0:
            angle = 360 - abs(angle)
        angles[360 - angle].append(point)

    res = len(angles.values())

    if res > maxp:
        maxp = res
        bestp = posX, posY
        bestangles = angles

angles = bestangles

print("BEST POINT " + str(bestp))
print(angles)

all_angles = sorted(angles.keys())
# print(all_angles)

for a in all_angles:
    angles[a] = sorted(angles[a], key=functools.partial(dist, bestp))


# print(angles[0.0])
i = 0
r = 0
f = True
c_p = 0

last = all_angles.pop()
all_angles.insert(0, last)

coords = []
print(all_angles)
while i < 200:

    # if i == 0:
        # print(r)
    # print(c_p)
    if (len(all_angles) == 0):
        continue
    # print(all_angles[0]
    # print(all_angles[c_p])
    if r >= all_angles[c_p]:
        if len(angles[all_angles[c_p]]) > 0:
            coord = angles[all_angles[c_p]].pop(0)
            # if i == 0:
            coords.append(coord)
            i+= 1
            c_p += 1
            if c_p >= len(all_angles):
                c_p = 0
        else:
            all_angles.pop(0)
            c_p -= 1

    if (r < 0):
        r =  360 - r
    r -= 0.0001

# g = [['.' for i in range(19)] for j in range(6)]
# for ind, c in enumerate(coords):
#     x,y = c
#     g[y][x] = str(ind)
#
# print("\n".join(["".join(i) for i in g]))
#
print(coords[199])
#









# print(output)
