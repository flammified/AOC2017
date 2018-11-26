import math

a = 0
b = 0
c = 0
d = 0
f = 0
g = 0
h = 0

b = 105700
c = 122700

for b in range(b, c + 1, 17):
    f = True
    for d in range(2, int(math.sqrt(b))):
        if (b % d) == 0:
            f = False
            break

    if !f:
        h += 1

print(h)
