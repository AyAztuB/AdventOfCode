def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.strip().split(',') for l in f if len(l) > 1)
    return res

def __get_list_of_num(l):
    u = l.split('-')
    return list(range(int(u[0]), int(u[1])+1))

def __is_containing(l1, l2):
    return all(u in l2 for u in l1) or all(u in l1 for u in l2)

def part1():
    return sum(__is_containing(__get_list_of_num(l[0]), __get_list_of_num(l[1])) for l in load_data())

print(part1())

def __is_overlap(l1, l2):
    return any(u in l2 for u in l1) or any(u in l1 for u in l2)

def part2():
    return sum(__is_overlap(__get_list_of_num(l[0]), __get_list_of_num(l[1])) for l in load_data())

print(part2())

