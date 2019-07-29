from torch import nn
import torch
import numpy as np
import data_dlt as dlt
import math

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
    print(line)
    