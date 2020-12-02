from collections import defaultdict


total =0
total2 = 0
with open('input.txt', 'r') as f:
    t = f.read()
    for line in t.splitlines():
        print(line)
        b, p = line.split(":")
        r, letter = b.split(" ")
        s, e = r.split("-")
        s = int(s)
        e = int(e)

        count = defaultdict(int)
        for c in p:
            count[c] += 1

        print(s, e)
        if count[letter] >=s and count[letter] <= e:
            # print(r, letter, p)
            total += 1

        if (p[s] == letter and p[e] != letter) or (p[s] != letter and p[e] == letter):
            total2 +=1


print(total2)
