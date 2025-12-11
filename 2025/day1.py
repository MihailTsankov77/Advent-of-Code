def read():    
    return map(lambda x: int(x[1: len(x)+1]) if x[0] == "R" else -int(x[1: len(x)+1]) ,open('./inputs/day1.txt', 'r').read().split('\n'))

def part1():
    count = 0
    rotation = 50
    for i in read():
        rotation =  rotation + i % 100
        count += ((rotation // 100) * 100 == rotation)


    print(count)

def part2():
    count = 0
    rotation = 50
    for i in read():
        dir = 1 if i > 0 else -1
        for _ in range(abs(i)):
            rotation += dir
            if rotation == 100:
                rotation = 0
            if rotation == -1:
                rotation =99
            if rotation == 0:
                count +=1

    print(count)