import numpy as np
import torch
import data_dlt
from torch.autograd import Variable
import time
import matplotlib.pyplot as plt
from torch import nn
from torch.utils.data import DataLoader

def data_tf(x):
    x = np.array(x, dtype='float32') / 35
    x = (x - 0.5) / 0.5
    x = x.reshape((-1,))
    x = torch.from_numpy(x)
    return x

net = nn.Sequential(
nn.Linear(2100, 1000),
nn.ReLU(),
nn.Linear(1000, 500),
nn.ReLU(),
nn.Linear(500, 200),
nn.ReLU(),
nn.Linear(200, 10),
)

criterion = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(net.parameters(),lr=1e-3)

#Training
line = data_dlt.get_dim()[0]-304
start = time.time()
for e in range(100):
    train_loss = 0
    line = line+3
    im = Variable(data_tf(data_dlt.get_trainsX(line)), requires_grad=True)
    #label  = Variable(torch.from_numpy(data_dlt.get_trainsY(line)).cuda(0))
    label  = Variable(torch.from_numpy(data_dlt.get_trainsY(line)))
    #print(data_dlt.get_trainsX(line))
    print(im)
    out = net(im)
    print(out.shape)
    #print(label.shape)
    print(label)
    print('--------------------------------------------------')

    #loss = criterion(out, label)

