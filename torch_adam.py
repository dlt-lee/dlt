from __future__ import print_function
import torch
import data_dlt
import numpy as np
from torch import nn
from torch.autograd import Variable
import time
import matplotlib.pyplot as plt
print(data_dlt.get_dim()[0])
print(data_dlt.get_trainsX(500).reshape((-1,)))
print(data_dlt.get_trainsY(688))

def data_tf(x):
    x = np.array(x, dtype='float32') / 35
    x = (x - 0.5) / 0.5
    x = x.reshape((-1,))
    x = torch.from_numpy(x)
    return x

net = nn.Sequential(
    nn.Linear(2100, 200),
    nn.ReLU(),
    nn.Linear(200, 20),
    nn.ReLU(),
    nn.Linear(20, 7),
    )

criterion = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(net.parameters(), lr=1e-3)

#Training
line = data_dlt.get_dim()[0]-304
start = time.time()
for e in range(100):
    train_loss = 0
    line = line+3
    #print(line)
    im = Variable(data_tf(data_dlt.get_trainsX(line)),requires_grad=True)
    label = Variable(data_tf(data_dlt.get_trainsY(line)),requires_grad=True)
    #print(data_tf(data_dlt.get_trainsX(line)))
    #forward
    out = net(im)
    print(out)
    print(label)
    print('---------------------------------------------------')
    loss = criterion(out,label)
    #backward
    #optimizer.zero_grad()
    #loss.backward()
    #optimizer.step()

    #train_loss += loss.data[0]

end = time.time()
print(data_dlt.get_trainsX(line).shape)
print(label)
print (end-start)
