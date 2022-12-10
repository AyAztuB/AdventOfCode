import os

os.system("aoc d -d 10 -y 2022 -I -i input.txt -o")

def submition(answer, part):
    print(answer)
    os.system(f"aoc s {part} {answer} -d 10 -y 2022")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def __increase_answer(x, turn):
    return turn * x if turn in [20, 60, 100, 140, 180, 220] else 0

def part1():
    data = load_data()
    x = 1
    answer = 0
    turn = 0
    limits = [20, 60, 100, 140, 180, 220]
    for l in data:
        instruction = l.split()
        if instruction[0] == "noop":
            turn += 1
            answer += __increase_answer(x, turn)
        elif instruction[0] == "addx":
            turn += 1
            answer += __increase_answer(x, turn)
            turn += 1
            answer += __increase_answer(x, turn)
            x += int(instruction[1])
    return answer

submition(part1(), 1)

def __draw_CRT(x, turn, CRT):
    CRT[turn//40][turn%40] = '#' if abs(x-(turn % 40)) <= 1 else ' '

def part2():
    data = load_data()
    CRT = [[None for _ in range(40)] for _ in range(6)]
    x = 1
    turn = 0
    for l in data:
        instruction = l.split()
        if instruction[0] == "noop":
            __draw_CRT(x, turn, CRT)
            turn += 1
        elif instruction[0] == "addx":
            __draw_CRT(x, turn, CRT)
            turn += 1
            __draw_CRT(x, turn, CRT)
            turn += 1
            x += int(instruction[1])
    return CRT

CRT = part2()
for i in range(6):
    print(''.join(CRT[i]))







