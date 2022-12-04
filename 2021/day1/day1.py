from os import system

def submit(answer, part):
    system(f"echo {answer} && aoc s {part} {answer} -y 2021 -d 1")

def load_data():
    data = []
    with open('input.txt', 'r') as f:
        data.extend(int(l.strip()) for l in f)
    return data

def part1():
    data = load_data()
    return sum(data[i] > data[i-1] for i in range(1,len(data)))

submit(part1(), 1)

def part2():
    data = load_data()
    sums = [data[i] + data[i + 1] + data[i + 2] for i in range(len(data) - 2)]
    changes = [sums[1] - sums[0]]
    changes.extend(sums[i] - sums[i-1] for i in range(1, len(sums)))
    return sum(change > 0 for change in changes)

submit(part2(), 2)



