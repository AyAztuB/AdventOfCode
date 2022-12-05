from os import system

system("aoc d -d 2 -y 2021 -I -i input.txt -o")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def submit(answer, part):
    system(f"echo {answer} && aoc s {part} {answer} -d 2 -y 2021")

def part1():
    data = load_data()
    h,d = 0,0
    for l in data:
        x = int(l.split()[1])
        if l.startswith("forward"):
            h+=x
        elif l.startswith("up"):
            d-=x
        elif l.startswith("down"):
            d+=x
    return h*d

submit(part1(), 1)

def part2():
    data = load_data()
    position, depth, aim = 0,0,0
    for l in data:
        action, amount = l.split(' ')
        if action == "forward":
            position += int(amount)
            depth += int(amount)*aim
        elif action == "down":
            aim += int(amount)
        elif action == "up":
            aim -= int(amount)
    return position*depth

submit(part2(), 2)


