import numpy as np
import torch
from torch import nn
from torch.autograd import Variable

def vgg_block(num_convs, in_channels, out_channels):
    net = [nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1),nn.ReLU(True)]
    for i in range(num_convs-1):
        net.append(nn.Conv2d(out_channels, out_channels, kernel_size=3, padding=1))
        net.append(nn.ReLU(True))

    #pool:
    net.append(nn.MaxPool2d(2, 2))

    return nn.Sequential(*net)

block_demo = vgg_block(10, 1, 1)
print(block_demo)