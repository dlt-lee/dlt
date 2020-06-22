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

train_set = MNIST('./data', train=True,transform=data_tf,download=True)
test_set = MNIST('./data', train=False,transform=data_tf,download=True)
#print(train_set)
#print(type(train_set))

criterion = nn.CrossEntropyLoss()

train_data = DataLoader(train_set, batch_size=64, shuffle=True)
print(np.array(train_data).shape)
print(type(np.array(train_data)))

net = nn.Sequential(
    nn.Linear(784, 200),
    nn.ReLU(),
    nn.Linear(200, 10),
)

sqrs = []
vs = []
for param in net.parameters():
    sqrs.append(torch.zeros_like(param.data))
    vs.append(torch.zeros_like(param.data))
t = 1
losses = []
idx = 0

start = time.time()
for im, label in train_data:
    print(im)
    print(label)
    print(im.shape)
    print(label.shape)
    print("-----------------------------------------------------------------------------")