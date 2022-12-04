def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.strip() for l in f)
    return res

def __sum_for_each():
    res = []
    data = load_data()
    _ligne = 0
    for l in data:
        if len(l):
            _ligne += int(l)
        else:
            res.append(_ligne)
            _ligne = 0
    return res

def __get_max(l):
    if not len(l):
        return None
    res = l[0]
    for i in range(1, len(l)):
        if l[i] > res:
            res = l[i]
    return res

def part1():
    return __get_max(__sum_for_each())

print(part1())

def __swap_triple(t):
    if t[0] > t[1]:
        t[0],t[1] = t[1],t[0]
        if t[1] > t[2]:
            t[1],t[2] = t[2],t[1]

def __get_max_3(l):
    if not len(l):
        return None
    res = [l[0] for _ in range(3)]
    for i in l:
        res[0] = max(res[0], i)
        __swap_triple(res)
    return res

def __sum_triple(t):
    return None if t is None else t[0]+t[1]+t[2]

def part2():
    return __sum_triple(__get_max_3(__sum_for_each()))

print(part2())



