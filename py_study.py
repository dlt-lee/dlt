print(100+200+300)
print('hello,	world	and	hello	deep	learning!')
print(type(3))
print(type('class'))
print(int(1.23e9))
print(1.2e-2)
s = 'abc'
print(s)
print(len(s))
print('I\'m \"OK\"')
a = 'hello'
b = 'world'
c = a + ' like ' + b
print(c)
d = 'a: {}, b: {}'.format(23, 34)
print(d)
s = 'hello'
print(s.capitalize())
print(s.upper())
print(s.rjust(7))
print(s.center(7))
print(s.replace('e','o'))
print(' world       '.strip())
t=True
f=False
print(type(t))
print(3>8)
print(t and f)
print(t or f)
print(not t)
print(t != f)
print('I\'m \"OK\"')
a = 'ABC'
b = a
a = 'XYZ'
print(b)
xs=[3,1,2]
print(xs,xs[2])
print(xs[-1])
print(len(xs))
xs[2]='foo'
print(xs.pop())
print(xs)
print(xs.append('end'))
print(xs)
L = [
['Apple', 'Google', 'Microsoft'],
['Java', 'Python', 'Ruby', 'PHP'],
['Adam', 'Bart', 'Lisa']
]
print(L[0][0],L[1][1],L[2][2])
nums = [0, 1, 2, 3, 4]
print(nums)
print(nums[2:4])
print(nums[2:])  
print(nums[:2])
print(nums[:])
print(nums[:-1])
#nums[2:4]=[8,9]
print(nums)
nums=nums[:2]+nums[3:]
print(nums)
animals = ['cat', 'dog', 'monkey']
for animal in animals:
    print(animal)
for idx, animal in enumerate(animals):
    print('#{}: {}'.format(idx + 1, animal))
nums = [0, 1, 2, 3, 4]
squares = []
for x in nums:
    squares.append(x**2)
print(squares)
squares = [x**2 for x in nums]
print(squares)
total =0
for x in range(101):
    #print(x)
    total=total+x
print(total)
animals = {'cat', 'dog'}
print('cat' in animals)
print('fish' in animals)
print(len(animals))
animals.add('fish')
print(animals)
animals.add('cat')
print(animals)
animals.remove('cat')
print(animals)
d = {'cat': 'cute', 'dog': 'furry'}
print(d['cat'])
print('furry' in d)
print('dog' in d)
d['fish']='wet'
print(d)
print(d.get('fish'))
print(d.get('monkey','N/A'))
del d['fish']
print(d.get('fish'))



d = {(x, x + 1): x for x in range(10)}
t = (5, 6)
print(type(d))
print(d[t])
print(d)
print(d[(3,4)])


def abs_fun(x):
    if x >= 0:
        return x
    else:
        return -x
print(abs_fun(3))
print(abs_fun(-2))

#import numpy as np
a = np.array([1,2,3,4,8,12])
print(type(a))
print(a.shape)
print(a[0],a[1],a[2])
a[1] = 3
print(a)
b = np.array([[1,2,3,],[4,5,6]])
print(b.shape)
print(b)

a = np.array([[1,2,3,4], [5,6,7,8], [9,10,11,12]])
print(a)
b = a[:2,1:3] #row:0,1 clum:1,2
print(b)
row_r1 = a[1,:] #row:1 clum:all
print(row_r1)
print(a[0,0],a[1,1])
bool_idx = a > 6
print(a)
print(bool_idx)
print(a[bool_idx])

x = np.array([[1,2],[3,4]])
print(x)
print(np.sum(x))
print(np.sum(x,axis=0))
print(np.sum(x,axis=1))