from collections import defaultdict


total = 0
lines = 0
with open('input.txt', 'r') as f:
    t = f.read()

    g = defaultdict(int)
    total = 0
    for l in t.splitlines():
        l = l.strip()
        if len(l) != 0:
            lines += 1
            for c in l:
                g[c] += 1


        else:
            print(g, lines)
            for k in g.keys():
                if g[k] == lines:
                    print(k)
                    total += 1
            g = defaultdict(int)
            lines = 0

print(g)
for k in g.keys():
    if g[k] == lines:
        total += 1
g = defaultdict(int)
chars = {}
g = {}
print(total)
