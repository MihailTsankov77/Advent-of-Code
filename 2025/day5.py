def read(): 
    rows = open('./inputs/day5.txt', 'r').read().split('\n')

    intervals = []
    requests = []
    is_intervals = True
    for row in rows:
        if row == '':
            is_intervals = False
            continue

        if is_intervals:
            parts = row.split('-')
            intervals.append((int(parts[0]), int(parts[1])))
        else:
            requests.append(int(row))

    intervals = sorted(intervals, key=lambda x: x[0])

    for i in range(1, len(intervals)):
        if intervals[i][0] <= intervals[i - 1][1]:
            intervals[i] = (intervals[i - 1][0], max(intervals[i - 1][1], intervals[i][1]))
            intervals[i - 1] = None

    intervals = [iv for iv in intervals if iv is not None]

    return intervals, requests

def part1():
    intervals, requests = read()
    count = 0
    for req in requests:
        for interval in intervals:
            if interval[0] <= req <= interval[1]:
                count += 1
                break

    print(count)

def part2():
    intervals, _ = read()
    count = 0
    for interval in intervals:
        count += interval[1] - interval[0] + 1

    print(count)
