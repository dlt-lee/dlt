from torch.autograd import Variable
import torch
import numpy as np
import matplotlib.pyplot as plt
# build numpy ndarray
numpy_tensor = np.random.randn(10, 20)
print(numpy_tensor)
print(type(numpy_tensor))
pytorch_tensor1 = torch.tensor(numpy_tensor)
pytorch_tensor2 = torch.from_numpy(numpy_tensor)
print(type(pytorch_tensor1))
print(type(pytorch_tensor2))
numpy_array = pytorch_tensor1.numpy()
print(type(numpy_array))
numpy_array = pytorch_tensor1.cpu().numpy()
print(type(numpy_array))
gpu_tensor = torch.randn(10,20).cuda(0)
cpu_tensor = gpu_tensor.cpu()
x_tensor = torch.randn(10,5)
y_tensor = torch.randn(10,5)
print(x_tensor)
print(y_tensor)
z_tensor = torch.add(x_tensor,y_tensor)
print(z_tensor)
x = Variable(x_tensor,requires_grad=True)
y = Variable(y_tensor,requires_grad=True)
z = torch.sum(x+y)
print(z.data)
print(z.grad_fn)
z.backward()
print(x.grad)
print(y.grad)
x = Variable(torch.FloatTensor([2]), requires_grad=True)
y = x**2
y.backward()
print(x.grad)

x = np.arange(-3,3.01,0.1)
y = x**2
plt.plot(x,y)
plt.plot(2,4,'ro')
plt.show()