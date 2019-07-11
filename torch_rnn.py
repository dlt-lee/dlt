import torch
import data_dlt
from torch import nn
from torch.autograd import Variable

embeds = nn.Embedding(30, 5)
embeds.weight

line = data_dlt.get_dim()[0]
print(line)
#print(data_dlt.get_trainsY(line))
print(data_dlt.get_trainsX(line).shape)
print(data_dlt.get_trainsX(line))

#embeds.weight.data = orch.from_numpy(data_dlt.get_trainsX(line))
#embeds.weight
