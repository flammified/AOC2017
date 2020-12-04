with open('input.txt', 'r') as f:
    text = f.read()
    grid = {}
    loc = []
    h = len(text.splitlines())
    for y, line in enumerate(text.splitlines()):
        width = len(line)
        for x, c in enumerate(line):
            grid[x, y] = c

def count_trees(dx, dy):
    x = 0
    y = 0
    trees = 0
    while y < h:
        if (x % width, y) in grid:
            if grid[x % width, y] == '#':
                trees += 1
        x += dx
        y += dy

    return trees

s = [
    count_trees(1, 1),
    count_trees(3,1),
    count_trees(5,1),
    count_trees(7,1),
    count_trees(1,2)
]

res = 1
for i in s:
    res *= i
print(res)
