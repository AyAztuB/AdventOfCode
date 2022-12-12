import itertools
from os import system

system("aoc d -d 12 -y 2022 -I -i input.txt -o")

def load_data():
    data = []
    with open("input.txt", "r") as f:
        data.extend(list(l.strip()) for l in f)
    return data

def submit(part, answer):
    system(f"echo {answer} && aoc s {part} {answer} -d 12 -y 2022")

def __get_neighboorg(data, pos):
    res = []
    y, x = pos
    if y > 0 and data[y-1][x] <= 1+ data[y][x]:
        res.append((y-1, x))
    if y < len(data) - 1 and data[y+1][x] <= 1+ data[y][x]:
        res.append((y+1, x))
    if x > 0 and data[y][x-1] <= 1+ data[y][x]:
        res.append((y, x-1))
    if x < len(data[0]) - 1 and data[y][x+1] <= 1+ data[y][x]:
        res.append((y, x+1))
    return res

def __start(data, part):
    return [((i, j), 0) for i, j in itertools.product(range(len(data)), range(len(data[0]))) if data[i][j] == part]

def __get_ord(data):
    a = ord('a')
    res = [[0 for _ in range(len(data[0]))] for _ in range(len(data))]
    for i, j in itertools.product(range(len(data)), range(len(data[0]))):
        if data[i][j] == 'S':
            res[i][j] = 1
        elif data[i][j] == 'E':
            res[i][j] = 26
        else:
            res[i][j] = ord(data[i][j]) - a + 1
    return res


def __a_star(part):
    data = load_data()
    _ord = __get_ord(data)
    opened = __start(data, part)
    closed = set()
    while len(opened):
        (i, j), distance = opened.pop(0)
        if (i, j) not in closed:
            closed.add((i, j))
            if data[i][j] == 'E':
                return distance
            for n in __get_neighboorg(_ord, (i,j)):
                opened.append((n, distance+1))
    print("ERROR!")

def part1():
    return __a_star('S')

submit(1, part1())

def part2():
    return __a_star('a')

submit(2, part2())





