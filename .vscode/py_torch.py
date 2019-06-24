
from __future__ import print_function
import torch
import numpy as np

x = torch.rand(5, 3)
print(x)

print(torch.cuda.is_available())

print(np.sum([365,365]))
