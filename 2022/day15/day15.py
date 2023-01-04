import itertools
from os import system

system("aoc d -d 15 -y 2022 -I -i input.txt -o")

def load_data():
    data = []
    with open("input.txt", "r") as f:
        data.extend(l.strip().replace('\n','') for l in f if len(l) > 1)
    return data

def submit(part, answer):
    system(f"echo {answer} && aoc s {part} {answer} -d 15 -y 2022")

def __get_input():
    data = load_data()
    S = set()
    B = set()
    for l in data:
        w = l.split()
        sx,sy = int(w[2][2:-1]),int(w[3][2:-1])
        bx,by = int(w[8][2:-1]),int(w[9][2:])
        d = abs(sx-bx) + abs(sy-by)
        S.add((sx, sy, d))
        B.add((bx, by))
    return S,B

def __valid(x,y,S):
    return all(abs(x-sx)+abs(y-sy) > d for sx, sy, d in S)

def part1():
    S,B = __get_input()
    y = int(2e6)
    ans = sum(not __valid(x, y, S) and (x, y) not in B for x in range(-int(1e7), int(1e7)))
    submit(1,ans)
    return ans

def part2():
    S,B = __get_input()
    for (sx,sy,d) in S:
        for dx in range(d+2):
            dy = (d+1)-dx
            for _x,_y in [(-1,-1),(-1,1),(1,-1),(1,1)]:
                x = sx + (dx*_x)
                y = sy + (dy*_y)
                if (0<=x<=4000000 and 0<=y<=4000000) and __valid(x, y, S):
                    submit(2, x*4000000+y)
                    return x*4000000+y
    print("ERROR : answer not found !")
    return None

if __name__ == "__main__":
    part1()
    part2()






