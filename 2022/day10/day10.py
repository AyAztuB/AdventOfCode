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

def __draw_CRT(x, turn, CRT):
    CRT[turn//40][turn%40] = '#' if abs(x-(turn % 40)) <= 1 else ' '

def __print_CRT(CRT):
    for i in range(6):
        print(''.join(CRT[i]))


def __challenge():
    data = load_data()
    x = 1
    answer = 0
    turn = 0
    CRT = [[None for _ in range(40)] for _ in range(6)]
    for l in data:
        instruction = l.split()
        if instruction[0] == "noop":
            __draw_CRT(x, turn, CRT)
            turn += 1
            answer += __increase_answer(x, turn)
        elif instruction[0] == "addx":
            __draw_CRT(x, turn, CRT)
            turn += 1
            answer += __increase_answer(x, turn)
            __draw_CRT(x, turn, CRT)
            turn += 1
            answer += __increase_answer(x, turn)
            x += int(instruction[1])
    return answer, CRT

def part1():
    return __challenge()[0]

submition(part1(), 1)

def part2():
    __print_CRT(__challenge()[1])

part2()






