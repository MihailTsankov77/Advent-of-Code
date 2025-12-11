import re 

def read():    
    rangs = open('./inputs/day2.txt', 'r').read().split('\n')[0].split(',')
    return map(lambda x: range(int(x.split('-')[0]), int(x.split('-')[1]) + 1)  ,rangs)

count = 0
for r in read():
    for _id in r:
        id = str(_id)
        middle = len(id) // 2 
        for index in range(middle+1):
            part = id[:index]
            if re.match(f"^({re.escape(part)})+$", id):
                count += _id
                break

print(count)
