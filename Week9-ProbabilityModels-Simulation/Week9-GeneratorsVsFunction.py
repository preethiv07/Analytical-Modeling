# create Simpy1.py file. tjhe functions specified in the python file can be used as from simpy1
i = 0
#import six


def function(n):
    for i in range(n):
        return i


def generator(n):
    while True:
        for i in range(n):
            yield i


ourfun = function(3)
ourgen = generator(3)

print('fun', ourfun)
#print('gen', ourgen)
print('next gen1',next(ourgen))
print('next gen',next(ourgen))
print('next gen3',next(ourgen))
#next(generator(3))

