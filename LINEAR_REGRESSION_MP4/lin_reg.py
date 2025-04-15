import numpy as np
from sklearn.model_selection import train_test_split

data = np.loadtxt('./data/dane4.txt')

x = data[:, 0]
y = data[:, 1]

x_train, x_test, y_train, y_test = train_test_split(x,y,test_size=0.2,random_state=1)

x_mean = np.mean(x_train)
y_mean = np.mean(y_train)


a = np.sum((x_train - x_mean) * (y_train - y_mean)) / np.sum((x_train -  x_mean) ** 2)
b = y_mean - a * x_mean


print("Linear mmdel : y = ", a, " * x +", b,)



y_pred = a * x_test + b
mse = np.mean((y_test - y_pred) ** 2)
print("MSE: ",mse)




print('-------------------2-------------------')
A_train = np.column_stack((x_train**2, x_train, np.ones_like(x_train)))

theta = np.linalg.inv(A_train.T @ A_train) @ A_train.T @ y_train

a, b, c = theta
print("Quadratic Model: y = ",a," * x^2 + ",b,"* x +", c)

A_test = np.column_stack((x_test**2, x_test, np.ones_like(x_test)))
y_pred = A_test @ theta

mse = np.mean((y_test - y_pred) ** 2)
print(f"MSE (Model 2): {mse:.5f}")

