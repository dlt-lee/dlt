from torch import nn
import torch
import numpy as np
import data_dlt as dlt

rows = dlt.get_dim()[0]
#print(rows)

line=rows-3
#print(dlt.get_trainsX(line))
#print(dlt.get_trainsX(line).shape)
#print(dlt.get_trainsY(line))
train_x = dlt.get_trainsX(line)
train_y = dlt.get_trainsY(line)
for i in range(200) :
    line = line-3
    temp_data_x = dlt.get_trainsX(line)
    temp_data_y = dlt.get_trainsY(line)
    train_x = np.concatenate((train_x,temp_data_x),axis=1)
    train_y = np.concatenate((train_y,temp_data_y),axis=1)
    print(train_x.shape)
    print(temp_data_y.shape)