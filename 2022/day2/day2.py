def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.strip() for l in f if len(l) > 3)
    return res

def part1():
    data = load_data()
    res = 0
    ecart = ord('A')
    for l in data:
        x = ord(l[2]) - ecart - 22
        y = ord(l[0]) - ecart + 1
        res += x + (3 if x == y else (6 if x - 1 == y % 3 else 0))
    return res

print(part1())

def part2():
    data = load_data()
    res = 0
    char = {'X': 0, 'Y': 3, 'Z': 6}
    win = {0: -1, 3: 0, 6: 1}
    a = ord('A')
    for l in data:
        x = char[l[2]]
        res += x + ((ord(l[0]) - a + win[x]) % 3) + 1
    return res

print(part2())





