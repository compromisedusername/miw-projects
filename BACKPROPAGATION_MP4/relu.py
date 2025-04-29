import numpy as np
import matplotlib.pyplot as plt


def relu(x):
    return np.maximum(0, x)


def relu_derivative(x):
    return np.exp(x) / (np.exp(x) + 1)


P = np.arange(-4, 4.1, 0.1)
T = P**2 + (np.random.rand(len(P))-0.5)

S1 = 2

W1 = np.random.rand(S1, 1) - 0.5
B1 = np.random.rand(S1, 1) - 0.5

W2 = np.random.rand(1, S1) - 0.5
B2 = np.random.rand(1) - 0.5

lr = 0.001

P = P.reshape(1, -1)
T = T.reshape(1, -1)

for epoka in range(1, 1000):
    X = W1 @ P + B1 @ np.ones((1, P.shape[1]))
    A1 = relu(X)
    A2 = W2 @ A1 + B2

    E2 = T - A2
    E1 = W2.T @ E2

    dW2 = lr * E2 @ A1.T
    dB2 = lr * E2 @ np.ones((E2.shape[1], 1))

    dZ = relu_derivative(X) * E1

    dW1 = lr * dZ @ P.T
    dB1 = lr * dZ @ np.ones((P.shape[1], 1))

    W2 += dW2
    B2 += dB2.flatten()
    W1 += dW1
    B1 += dB1

    if epoka % 5 == 0:
        plt.clf()
        plt.plot(P.flatten(), T.flatten(), 'r*', label='Target')
        plt.plot(P.flatten(), A2.flatten(), label='A2 - output')
        plt.title(f"Epoka {epoka}")
        plt.legend()
        plt.pause(0.05)

plt.show()
