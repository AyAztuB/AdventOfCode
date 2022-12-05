import os

os.system("aoc d -d 5 -y 2022 -I -i input.txt -o")

def submition(answer, part):
    print(answer)
    os.system(f"aoc s {part} {answer} -d 5 -y 2022")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def __get_start(data):
    i = 0
    while len(data) < 4 or data[i][:4] != "move":
        i+=1
    return i

def __create_stacks(data):
    i = 0
    stacks = []
    end = __get_start(data) -1
    while i < end:
        line = data[i]
        l = (len(line)+1)//4
        while len(stacks) < l:
            stacks.append([])
        for k in range(len(stacks)):
            if k*4+1 < len(line) and line[k*4+1] != ' ':
                stacks[k].insert(0, line[k*4+1])
        i+=1
    return stacks

def __assembly(popall):
    data = load_data()
    S = __create_stacks(data)
    #print(S)
    for i in range(__get_start(data), len(data)):
        line = data[i].split()
        if len(line) == 6:
            nb = int(line[1])
            src = S[int(line[3])-1]
            dst = S[int(line[5])-1]

            if len(src) >= nb:
                if popall:
                    tmp = [src.pop() for _ in range(nb)]
                    while len(tmp):
                        dst.append(tmp.pop())
                else:
                    for _ in range(nb):
                        dst.append(src.pop())
    return "".join(S[i].pop() for i in range(len(S)) if len(S[i]))

def part1():
    return __assembly(False)

submition(part1(), 1)

def part2():
    return __assembly(True)

submition(part2(), 2)


