import networkx

with open('input.txt', 'r') as f:
    t = f.read()


res = []
cur = {}
for p in t.splitlines():
    if len(p) == 0:
        res.append(cur)
        cur = {}
    else:
        for t in p.split():
            k, v = t.split(":")
            cur[k] = v
res.append(cur)

a = 0
for p in res:
    if len(p.keys()) == 8 or (len(p.keys()) == 7 and "cid" not in p):
        hex = list(map(chr, range(ord('a'), ord('f')+1))) + list(map(chr, range(ord('0'), ord('9')+1)))
        valid = [
            1920 <= int(p["byr"]) <= 2002,
            2010 <= int(p["iyr"]) <= 2020,
            2020 <= int(p["eyr"]) <= 2030,
            (150 <= int(p["hgt"][:-2]) <= 193 if p["hgt"][-2:] == "cm" else 59 <= int(p["hgt"][:-2]) <= 76) if p["hgt"][-2:] in ["cm", "in"] else False,
            all([c in hex for c in p["hcl"][1:]]) and p["hcl"][0] == "#",
            p["ecl"] in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
            p["pid"].isdigit() and len(p["pid"]) == 9
        ]
        print(p["hgt"][:-2])
        print(p)
        print(valid)

        if all(valid):
            a += 1



print(a)
