from torch import nn
import torch
import numpy as np
import data_dlt as dlt

rows = dlt.get_dim()[0]
print(rows)

line=rows
print(dlt.get_trainsX(line))
print(dlt.get_trainsX(line).shape)
print(dlt.get_trainsY(line))
