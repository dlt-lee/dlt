import numpy as np
import math
import data_dlt as dlt
from torch import nn
import matplotlib.pyplot as plt
import torch
import torchvision.datasets as dsets
import torchvision.transforms as transforms

# Hyper Parameters
TIME_STEP = 30
INPUT_SIZE = 7
LR = 0.01

class RNN(nn.Module):
    def __init__(self):
        super(RNN, self).__init__()

        self.rnn = nn.LSTM(
            input_size=INPUT_SIZE,
            hidden_size=64,
            num_layers=1,
            batch_first=True,
        )
        self.out = nn.Linear(64, 7)

    def forward(self, x):
        r_out, (h_n, h_c) = self.rnn(x, None)
        out = self.out(r_out[:, -1, :])
        return out

rnn = RNN()
print(rnn)

optimizer = torch.optim.Adam(rnn.parameters(), lr=LR)   # optimize all cnn parameters
loss_func = nn.CrossEntropyLoss()                       # the target label is not one-hotted

# training and testing
rows = dlt.get_dim()[0]-2  #move to last result
print(rows)
line = rows%3
n = (rows-line)/3-10
line = math.floor(rows-n*3)
print(line)

for i in range(math.floor(n)) :
    line = line+3
    train_x = torch.from_numpy(dlt.get_trainsX(line))
    train_y = torch.from_numpy(dlt.get_trainsY(line))
    #print(line)
    