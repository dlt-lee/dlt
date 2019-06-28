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
    #x = torch.from_numpy(x)
    return x

net = nn.Sequential(
nn.Linear(2100, 1000),
nn.ReLU(),
nn.Linear(1000, 600),
nn.ReLU(),
nn.Linear(600, 300),
nn.ReLU(),
nn.Linear(300, 100),
nn.ReLU(),
nn.Linear(100, 10),
)

criterion = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(net.parameters(),lr=1e-3)

#Build data
#print(data_dlt.get_dim())
train_data = np.array ([0])
line = data_dlt.get_dim()[0]-304
train_data = data_tf(data_dlt.get_trainsX(line))
train_data = np.hstack((train_data,data_dlt.get_trainsY(line)))
for i in range(100):
    line = line+3
    temp_data = data_tf(data_dlt.get_trainsX(line))
    temp_data = np.hstack((temp_data,data_dlt.get_trainsY(line)))
    train_data = np.vstack((train_data,temp_data))
#print(train_data.shape)
#print(train_data[200,:210])
#print(train_data[200,210:])
line = train_data.shape[0]
#print(line)
for j in range(0,line):
    im = Variable(torch.from_numpy(np.array(train_data[j:j+1,:2100],dtype='float32')), requires_grad=True)
    label = Variable(torch.from_numpy(np.array(train_data[j,2100:2101],dtype='int64')))
    print(im.shape)
    

    out = net(im)
    print(out)
    print(label)
    print('-----------------------------------------------------')

    loss = criterion(out, label)