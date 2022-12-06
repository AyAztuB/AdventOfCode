import os

os.system("aoc d -d 6 -y 2022 -I -i input.txt -o")

def submition(answer, part):
    print(answer)
    os.system(f"aoc s {part} {answer} -d 6 -y 2022")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def __get_index(nb):
    data = load_data()[0]
    for i in range(len(data)-(nb-1)):
        chars = data[i:i+nb]
        if len(set(chars)) == nb:
            return i+nb
    return len(data)


def part1():
    return __get_index(4)

submition(part1(), 1)

def part2():
    return __get_index(14)

submition(part2(), 2)


