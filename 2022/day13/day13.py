from os import system
from functools import cmp_to_key

system("aoc d -d 13 -y 2022 -I -i input.txt -o")

def load_data():
    data = []
    with open("input.txt", "r") as f:
        data = f.read().strip().split('\n\n')
    return data

def submit(part, answer):
    system(f"echo {answer} && aoc s {part} {answer} -d 13 -y 2022")

def __compare(p1,p2):
    if isinstance(p1, int) and isinstance(p2, int):
        return -1 if p1 < p2 else p1 != p2
    if isinstance(p1, list) and isinstance(p2, list):
        i = 0
        while i < len(p1) and i < len(p2):
            c = __compare(p1[i], p2[i])
            if c != 0:
                return c
            i += 1
        if i < len(p2):
            return -1
        return 1 if i < len(p1) else 0
    if isinstance(p1, int) and isinstance(p2, list):
        return __compare([p1], p2)
    return __compare(p1, [p2])

def __challenge(part):
    data = load_data()
    ans = part - 1
    packets = []
    for i,group in enumerate(data):
        p1,p2 = group.split('\n')
        p1 = eval(p1)
        p2 = eval(p2)
        packets.extend((p1, p2))
        if part == 1 and __compare(p1,p2) == -1:
            ans += 1+i
    if part == 1:
        return ans
    packets.extend(([[2]], [[6]]))
    packets = sorted(packets, key=cmp_to_key(lambda p1,p2: __compare(p1,p2)))
    for i,p in enumerate(packets):
        if p in [[[2]], [[6]]]:
            ans *= i+1
    return ans

def part1():
    submit(1, __challenge(1))

def part2():
    submit(2, __challenge(2))

if __name__ == "__main__":
    part1()
    part2()



