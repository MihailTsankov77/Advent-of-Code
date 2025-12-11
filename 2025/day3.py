def read(): 
    rows = open('./inputs/day3.txt', 'r').read().split('\n')

    return rows

def solution(n):
    answer = 0
    for row in read():
        biggest_nums = []
        for idx, st in enumerate(row):
            num = int(st)
            for b_idx in range(n):
                if len(biggest_nums) <= b_idx:
                    biggest_nums.append(num)
                    break
                
                if num > biggest_nums[b_idx] and idx < len(row) - n + b_idx + 1:
                    biggest_nums.insert(b_idx, num)
                    biggest_nums = biggest_nums[:b_idx + 1]
                    break
        
        num = biggest_nums[0]
        for b in biggest_nums[1:]:
            num = num * 10 + b
        answer += num

    print(answer)

def part1():
    solution(2)

def part2():
    solution(12)
