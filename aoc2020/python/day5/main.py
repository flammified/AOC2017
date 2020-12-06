with open('input.txt', 'r') as f:
    i = f.read().strip()


with open('test.txt', 'r') as f:
    t = f.read().strip()



r = 0

h = 0

all = {}

for l in i.splitlines():
    cur = list(range(0, 128))

    for c in l[:-3]:
        if c == 'F':
            # print(len(cur) /2)
            cur = cur[:int(len(cur) / 2)]
        else:
            # print(len(cur)/2)
            cur = cur[int(len(cur) / 2):]

    cur2 = list(range(0, 8))
    r = 0


    for c in l[-3:]:
        # print(cur2)
        if c == 'L':
            cur2 = cur2[:int(len(cur2) /2)]
        else:
            cur2 = cur2[int(len(cur2) / 2):]
        # print(cur2)


    print(cur, cur2)
    id = cur[0] * 8 + cur2[0]
    if id > h:
        h = id

    all[id] = True


for i in range(0,850):
    print(all[i])
    if i not in all:
        print(i)
# print(h)
