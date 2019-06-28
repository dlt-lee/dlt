from __future__ import print_function
import numpy as np
import torch
from torchvision.datasets import MNIST 
from torch.utils.data import DataLoader
from torch import nn
from torch.autograd import Variable
import time
import matplotlib.pyplot as plt

def data_tf(x):
    x = np.array(x, dtype='float32') / 255
    x = (x - 0.5) / 0.5
    x = x.reshape((-1,))
    x = torch.from_numpy(x)
    return x

train_set = MNIST('./data', train=True, transform=data_tf, download=True)
test_set = MNIST('./data', train=False, transform=data_tf, download=True)

criterion = nn.CrossEntropyLoss()
train_data = DataLoader(train_set, batch_size=1, shuffle=True)
print(type(train_data))

net = nn.Sequential(
    nn.Linear(784, 200),
    nn.ReLU(),
    nn.Linear(200, 10),
    )

optimizer = torch.optim.Adam(net.parameters(), lr=1e-3)

start = time.time()
#print(type(train_set))
for e in range(2):
    train_loss = 0
    for im, label in train_data:
        im = Variable(im)
        #print(im.shape)
        #print(type(im))
        #print(im.shape)
        #label = Variable(label)
        #print(type(label))
        #print(label.shape)
        
        out = net(im)
        print(out)
        #print(type(out))
        #print(out.shape)
        print(label)
        loss = criterion(out, label)
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        print('--------------------------------------------------')
        #train_loss +=loss
    #print('epoch: {}, Train Loss: {:.6f}'.format(e, train_loss / len(train_data)))
end = time.time()
print('Time: {:.5f} s'.format(end - start))