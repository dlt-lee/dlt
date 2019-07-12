import torch
import data_dlt
from torch import nn
from torch.autograd import Variable

embeds = nn.Embedding(30, 7)
embeds.weight

line = data_dlt.get_dim()[0]
#print(line)
#print(data_dlt.get_trainsY(line))
print(data_dlt.get_trainsX(line).shape)
print(type(data_dlt.get_trainsX(line)))

embeds.weight.data = torch.from_numpy(data_dlt.get_trainsX(line))
print(embeds.weight)

CONTEXT_SIZE = 7
EMBEDDING_DIM = 30

