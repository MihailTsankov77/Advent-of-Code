def read(): 
    rows = open('./inputs/day4.txt', 'r').read().split('\n')

    return list(rows)


def part1():
    answer = 0
    matrix = read()
    for row_idx, row in enumerate(matrix):
        for column_idx, el in enumerate(row):
            if el != '@':
                continue

            neighbors = 0
            if row_idx > 0 and matrix[row_idx - 1][column_idx] == '@':
                neighbors += 1

            if row_idx < len(matrix) - 1 and matrix[row_idx + 1][column_idx] == '@':
                neighbors += 1

            if column_idx > 0 and matrix[row_idx][column_idx - 1] == '@':
                neighbors += 1

            if column_idx < len(row) - 1 and matrix[row_idx][column_idx + 1] == '@':
                neighbors += 1

            # -------------- diagonals --------------
            if row_idx > 0 and column_idx > 0 and matrix[row_idx - 1][column_idx - 1] == '@':
                neighbors += 1 
            
            if row_idx > 0 and column_idx < len(row) - 1 and matrix[row_idx - 1][column_idx + 1] == '@':
                neighbors += 1
            
            if row_idx < len(matrix) - 1 and column_idx > 0 and matrix[row_idx + 1][column_idx - 1] == '@':
                neighbors += 1
            
            if row_idx < len(matrix) - 1 and column_idx < len(row) - 1 and matrix[row_idx + 1][column_idx + 1] == '@':
                neighbors += 1

            if neighbors < 4:
                answer += 1
           

    print(answer)


def part2():
    answer = 0
    matrix = read()
    while True:
        new_lifts = 0
        for row_idx, row in enumerate(matrix):
            for column_idx, el in enumerate(row):
                if el != '@':
                    continue

                neighbors = 0
                if row_idx > 0 and matrix[row_idx - 1][column_idx] == '@':
                    neighbors += 1

                if row_idx < len(matrix) - 1 and matrix[row_idx + 1][column_idx] == '@':
                    neighbors += 1

                if column_idx > 0 and matrix[row_idx][column_idx - 1] == '@':
                    neighbors += 1

                if column_idx < len(row) - 1 and matrix[row_idx][column_idx + 1] == '@':
                    neighbors += 1

                # -------------- diagonals --------------
                if row_idx > 0 and column_idx > 0 and matrix[row_idx - 1][column_idx - 1] == '@':
                    neighbors += 1 
                
                if row_idx > 0 and column_idx < len(row) - 1 and matrix[row_idx - 1][column_idx + 1] == '@':
                    neighbors += 1
                
                if row_idx < len(matrix) - 1 and column_idx > 0 and matrix[row_idx + 1][column_idx - 1] == '@':
                    neighbors += 1
                
                if row_idx < len(matrix) - 1 and column_idx < len(row) - 1 and matrix[row_idx + 1][column_idx + 1] == '@':
                    neighbors += 1

                if neighbors < 4:
                    matrix[row_idx] = matrix[row_idx][:column_idx] + '.' + matrix[row_idx][column_idx + 1:]
                    new_lifts += 1

        if new_lifts == 0:
            break

        answer += new_lifts
           

    print(answer)


part2()