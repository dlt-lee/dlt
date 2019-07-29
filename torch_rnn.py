import numpy as np
import math
import data_dlt as dlt
from torch import nn
import matplotlib.pyplot as plt
import torch
import torchvision.datasets as dsets
import torchvision.transforms as transforms

rows = dlt.get_dim()[0]-2  #move to last result
print(rows)

line = rows%3
n = (rows-line)/3-10

line = math.floor(rows-n*3)
print(line)

for i in range(math.floor(n)) :
    line = line+3
    train_x = dlt.get_trainsX(line)
    train_y = dlt.get_trainsY(line)
    #print(line)
    