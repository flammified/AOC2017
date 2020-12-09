with open('input.txt', 'r') as f:
    t = f.read()



total = 0
l = []
valid = 0
inv = 0
for index, i in enumerate(t.splitlines()):
    # print(i)
    if len(l) == 25:
        # print(l)
        found = False
        for x in l:
            for y in l:
                if int(i) == (int(x) + int(y)):
                    found = True

        del l[0]
        l.append(int(i))

        # found = False

        if found:
            valid += 1
        else:
            print(i)
            # print(min(l) + max(l))
            inv = index
    else:
        l.append(int(i))
# print(valid)

l = [int(i) for i in t.splitlines()]

for i in range(0, inv):
    for end in range(i, inv):
        tl = l[i:end]
        s = sum(tl)
        # print(tl)
        if s == int(l[inv]):
            print("Found")
            print(min(tl) + max(tl))
