with open('input.txt', 'r') as f:
    s = f.read()
    l = []
    mem = {}
    for v in s.splitlines():
        l.append(int(v))
    for index, val in enumerate(l):
        for i2 in range(index + 1, len(l)):
            val2 = l[i2]
            if (val + val2):
                if val + val2 == 2020:
                    print("PART1", val * val2)
            for i3 in range(index + 1, len(l)):
                val3 = l[i3]
                if (val + val2 + val3) == 2020:
                    print("PART2", val * val2 * val3)
