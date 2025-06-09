import numpy as np
import matplotlib.pyplot as plt
a = np.loadtxt('danet.txt')
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM

x = a[0:40,[1,2,3]]
y = a[0:40,[0]]

#c = np.hstack([x, np.ones(y.shape)])
c = x
v = np.linalg.pinv(c) @ y

print(v)

model = Sequential()
model.add(LSTM(100, input_shape=(3, 1)))
model.add(Dense(1))
model.compile(loss='mean_squared_error', optimizer='adam')
model.fit(x, y, epochs=100, batch_size=1, verbose=2)

y1 = model.predict(x)

plt.plot(y, 'r-')
plt.plot(y1, 'k-')
plt.plot(v[0]*x[:,0] + v[1]*x[:,1] + v[2]*x[:,2],'g-')
plt.plot(x[:,0], 'b-')
plt.show()

