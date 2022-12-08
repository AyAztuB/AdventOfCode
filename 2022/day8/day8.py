import os

os.system("aoc d -d 8 -y 2022 -I -i input.txt -o")

def submition(answer, part):
    print(answer)
    os.system(f"aoc s {part} {answer} -d 8 -y 2022")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def __one_direction(data, row, col, direction):
    distance = 0
    rrow = row + direction[0]
    ccol = col + direction[1]
    while rrow < len(data) and rrow >= 0 and ccol < len(data[row]) and ccol >= 0:
        distance += 1
        if data[rrow][ccol] >= data[row][col]:
            return (False, distance)
        rrow += direction[0]
        ccol += direction[1]
    return (True, distance)

def __challenge():
    data = load_data()
    count_visible = 0
    mult_distances = 0
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for row in range(len(data)):
        for col in range(len(data[row])):
            all_direction = [__one_direction(data, row, col, directions[x]) for x in range(4)]
            if any(all_direction[i][0] for i in range(4)):
                count_visible += 1
            distance = 1
            for i in range(4):
                distance *= all_direction[i][1]
            mult_distances = max(mult_distances, distance)
    return (count_visible, mult_distances)

def part1():
    return __challenge()[0]

submition(part1(), 1)

def part2():
    return __challenge()[1]

submition(part2(), 2)


