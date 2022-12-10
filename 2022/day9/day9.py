import os

os.system("aoc d -d 9 -y 2022 -I -i input.txt -o")

def submition(answer, part):
    print(answer)
    os.system(f"aoc s {part} {answer} -d 9 -y 2022")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def __advance(H, T):
    _row = abs(H[0] - T[0])
    _col = abs(H[1] - T[1])
    if _row < 2 and _col < 2:
        return T
    Row = H[0]
    Col = H[1]
    if _row >= 2:
        Row = H[0]-1 if T[0]<H[0] else H[0]+1
    if _col >= 2:
        Col = H[1]-1 if T[1]<H[1] else H[1]+1
    return (Row, Col)

def part1():
    data = load_data()
    H = (0,0)
    T = (0,0)
    answer = {T}
    # movments in (row, col)
    directions = {'L': (0,-1), 'U': (-1,0), 'R': (0,1), 'D': (1,0)}
    for l in data:
        (direction, amount) = l.split()
        amount = int(amount)
        dir_tuple = directions[direction]
        for _ in range(amount):
            H = (H[0] + dir_tuple[0], H[1] + dir_tuple[1])
            T = __advance(H, T)
            answer.add(T)
    return len(answer)

submition(part1(), 1)

def part2():
    data = load_data()
    H = (0,0)
    T = [(0,0) for _ in range(9)]
    answer = {T[8]}
    # movments in (row, col)
    directions = {'L': (0,-1), 'U': (-1,0), 'R': (0,1), 'D': (1,0)}
    for l in data:
        (direction, amount) = l.split()
        amount = int(amount)
        dir_tuple = directions[direction]
        for _ in range(amount):
            H = (H[0] + dir_tuple[0], H[1] + dir_tuple[1])
            T[0] = __advance(H, T[0])
            for i in range(8):
                T[i+1] = __advance(T[i], T[i+1])
            answer.add(T[8])
    return len(answer)

submition(part2(), 2)


