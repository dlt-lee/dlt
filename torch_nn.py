from __future__ import print_function
import torch
import data_dlt
import numpy as np
print(data_dlt.get_dim())
print(data_dlt.get_trainsX(500).reshape((-1,)))
print(data_dlt.get_trainsY(500))

def data_tf(x):
    x = np.array(x, dtype='float32') / 35
    x = (x - 0.5) / 0.5 #
    x = x.reshape((-1,)) #
    x = torch.from_numpy(x)
    return x

