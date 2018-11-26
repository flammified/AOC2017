a = 0
b = 0
c = 0
d = 0
f = 0
g = 0
h = 0

b = 3
c = 10


for b in range(b, c, 1):
    f = True

    for d in range(2, b):

        for e in range(2, b):
            g = d
            g *= e
            g -= b

            print("----")
            print("E:", str(e))
            print("B:", str(b))
            print("G:", str(g))
            print("----")

            if g == 0:
                f = False
                break

    if f:
        h += 1

print(h)
