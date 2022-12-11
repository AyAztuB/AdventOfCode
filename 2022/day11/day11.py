from os import system

system("aoc d -d 11 -y 2022 -I -i input.txt -o")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(f.read().strip().split("\n\n"))
    return res

def submit(answer, part):
    system(f"echo {answer} && aoc s {part} {answer} -d 11 -y 2022")

def __init_monkey():
    data = load_data()
    monkey = []
    operation = []
    test = []
    true = []
    false = []
    for m in data:
        _, _items, _op, _test, _true, _false = m.split('\n')
        monkey.append([int(item) for item in _items.split(':')[1].split(',')])
        _op = ''.join(_op.split()[-3:])
        operation.append(lambda old, _op=_op:eval(_op))
        test.append(int(_test.split()[-1]))
        true.append(int(_true.split()[-1]))
        false.append(int(_false.split()[-1]))
    return (monkey, operation, test, true, false)

def __challenge(_range):
    monkey, operation, test, true, false = __init_monkey()
    modulo = 1
    for i in test:
        modulo *= i
    monkey_business = [0 for _ in range(len(monkey))]
    for _ in range(_range):
        for i in range(len(monkey)):
            for item in monkey[i]:
                monkey_business[i] += 1
                item = operation[i](item)
                item = item // 3 if (_range == 20) else item % modulo
                if item % test[i]:
                    monkey[false[i]].append(item)
                else:
                    monkey[true[i]].append(item)
            monkey[i] = []
    res = sorted(monkey_business)
    return res[-1] * res[-2]

def part1():
    return __challenge(20)

submit(part1(), 1)

def part2():
    return __challenge(10000)

submit(part2(), 2)


