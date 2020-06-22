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

#block_demo = vgg_block(3, 64, 128)
#print(block_demo)

#input_demo = Variable(torch.zeros(1, 64, 300, 300))
#output_demo = block_demo(input_demo)
#print(output_demo.shape)

def vgg_stack(num_convs, channels):
    net = []
    for n, c in zip(num_convs, channels):
        in_c = c[0]
        out_c = c[1]
        net.append(vgg_block(n, in_c, out_c))
        return nn.Sequential(*net)

vgg_net = vgg_stack((1, 1, 2, 2, 2), ((3, 64), (64, 128), (128, 256), (256, 512),(512, 512)))
print(vgg_net)
