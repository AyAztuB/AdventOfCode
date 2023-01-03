import itertools
from os import system

system("aoc d -d 14 -y 2022 -I -i input.txt -o")

def load_data():
    data = []
    with open("input.txt", "r") as f:
        data.extend(l.strip().replace('\n','') for l in f if len(l) > 1)
    return data

def submit(part, answer):
    system(f"echo {answer} && aoc s {part} {answer} -d 14 -y 2022")

def __get_input():
    data = load_data()
    res = set()
    for l in data:
        prev = None
        for point in l.split('->'):
            x,y = point.split(',')
            x,y = int(x), int(y)
            if prev is not None:
                _x, _y = x-prev[0], y-prev[1]
                dist = max(abs(_x), abs(_y))
                for i in range(dist+1):
                    x_res = prev[0]+ i*(-1 if _x < 0 else (_x > 0))
                    y_res = prev[1]+ i*(-1 if _y < 0 else (_y > 0))
                    res.add((x_res, y_res))
            prev = (x,y)
    floor = 2+ max(r[1] for r in res)
    lo_x = min(r[0] for r in res) - 2000
    hi_x = max(r[0] for r in res) + 2000
    for x in range(lo_x, hi_x):
        res.add((x, floor))
    return res,floor

def __challenge(part):
    data,floor = __get_input()
    for t in range(1000000):
        rock = (500, 0)
        while True:
            if rock[1]+1 >= floor and part == 1:
                return t
            if (rock[0], rock[1]+1) not in data:
                rock = (rock[0], rock[1]+1)
            elif (rock[0]-1, rock[1]+1) not in data:
                rock = (rock[0]-1, rock[1]+1)
            elif (rock[0]+1, rock[1]+1) not in data:
                rock = (rock[0]+1, rock[1]+1)
            else:
                break
        if rock == (500, 0) and part == 2:
            return t+1
        data.add(rock)
    return None

def part1():
    res = __challenge(1)
    if res is None:
        print("ERROR (part1 : None returned => answer not found)")
        return
    submit(1, res)

def part2():
    res = __challenge(2)
    if res is None:
        print("ERROR (part2 : None returned => answer not found)")
        return
    submit(2, res)

if __name__ == "__main__":
    part1()
    part2()






