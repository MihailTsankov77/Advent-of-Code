import re

def read(): 
    rows = open('./inputs/day6.txt', 'r').read().split('\n')
    matrix = [re.split(r"\s+", row.strip()) for row in rows]
    return matrix

def part1():
    matrix = read()
    count = 0

    for row in zip(*matrix):
        op = row[len(row) - 1]
        num = 0 if op == '+' else 1
        for el in row[:len(row) - 1]:
            if op == '*':
                num = num * int(el)
            else:
                num = num + int(el)
        count += num

    print(count)


def read2(): 
    rows = open('./inputs/day6.txt', 'r').read().split('\n')
    matrix = [list(row) for row in rows]
    return matrix

def part2():
    matrix = read2()
    count = 0
    nums = []
    op = ''
    for row in zip(*matrix):
        if not ''.join(row).strip():
            num = 0 if op == '+' else 1
            for el in nums:
                if op == '*':
                    num = num * el
                else:
                    num = num + el
            count += num
            nums = []
            op = ''
            continue
        
        cur_num = None
        for el in row:
            if el in ['*', '+']:
                op = el
            elif re.match(r"[0-9]", el):
                if cur_num is None:
                    cur_num = int(el)
                else:
                    cur_num = cur_num * 10 + int(el)
        if cur_num is not None:
            nums.append(cur_num)

    num = 0 if op == '+' else 1
    for el in nums:
        if op == '*':
            num = num * el
        else:
            num = num + el
    count += num
    print(count)
