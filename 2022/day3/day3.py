def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.strip() for l in f if len(l) > 1)
    return res

def __split_one(l):
    le = len(l)
    res1, res2 = "", ""
    for i in range(le//2):
        res1 += l[i]
    for i in range(le//2, le):
        res2 += l[i]
    return res1, res2

def __find_the_common(l):
    l1, l2 = l
    return "".join(i for i in l1 if i in l2)

def part1():
    data = load_data()
    res = 0
    a = ord('a')
    A = ord('A')
    for l in data:
        letter = __find_the_common(__split_one(l))
        #for i in letter:
        if len(letter):
            i = letter[0]
            res += ord(i) - A + 27 if i >= 'A' and i <= 'Z' else ord(i) - a + 1
    return res

print(part1())

def __get_common_3(l1, l2, l3):
    return next((i for i in l1 if i in l2 and i in l3), None)

def part2():
    data = load_data()
    a = ord('a')
    A = ord('A')
    res = 0
    for i in range(len(data)//3):
        l = __get_common_3(data[i*3], data[i*3+1], data[i*3+2])
        res += ord(l) - A + 27 if l >= 'A' and l <= 'Z' else ord(l) - a + 1
    return res

print(part2())




