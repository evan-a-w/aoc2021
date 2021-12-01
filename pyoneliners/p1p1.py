print((lambda x: sum([x[i] < x[i + 1] for i in range(0, len(x) - 1)]))([int(x.strip()) for x in open("inputs/p1p1.txt") if x.strip().isnumeric()]))
