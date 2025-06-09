from keras import layers
from keras import models
from keras.models import Sequential
from keras.datasets import mnist
import numpy as np
import matplotlib.pyplot as plt

import tensorflow as tf
from tensorflow import keras

from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score




def create_model(input_shape):
    model = models.Sequential()
    model.add(layers.Conv2D(16, (3, 3), padding='same', activation='relu', input_shape=input_shape))
    model.add(layers.MaxPooling2D((2, 2)))
    model.add(layers.Conv2D(8, (3, 3), padding='same', activation='relu'))
    model.add(layers.MaxPooling2D((2, 2)))
    model.add(layers.Conv2D(4, (3, 3), padding='same', activation='relu'))
    model.add(layers.UpSampling2D((2,2)))
    model.add(layers.Conv2DTranspose(8, (3, 3), padding='same', activation='relu'))
    model.add(layers.UpSampling2D((2,2)))
    model.add(layers.Conv2DTranspose(16, (3, 3), padding='same', activation='relu'))
    model.add(layers.Conv2D(1, (3, 3), padding='same', activation='sigmoid'))
    model.compile(optimizer='Adam', loss='categorical_crossentropy')
    model.summary()
    return model


(train_images, train_labels), (test_images, test_labels) = mnist.load_data()
train_images = train_images.reshape((60000, 28, 28, 1))
train_images = train_images.astype('float32') / 255
test_images = test_images.reshape((10000, 28, 28, 1))
test_images = test_images.astype('float32') / 255

model = create_model(input_shape=(28, 28, 1))
model.compile(optimizer='adam', loss='binary_crossentropy')
model.fit(train_images, train_images, epochs = 2, batch_size=64)
model.save_weights('wagi.weights.h5')


model1 = create_model(input_shape=(28, 28, 1))
model1.load_weights('wagi.weights.h5')
for _ in range(5):
    model1.pop()
model1.summary()


wynik1 = model1.predict(train_images[:500])

print(wynik1.shape)
print('wynik1 = {}'.format(wynik1.shape))
a,b,c,d = wynik1.shape
kod = wynik1.reshape(a, b*c*d)
print('kod = {}'.format(kod.shape))
print(kod)


for k in range(1, 11):
    knn = KNeighborsClassifier(n_neighbors=k)
    scores = cross_val_score(knn, kod, train_labels[:500], cv=5)
    print(f'K={k}, dokładność cross-val: {np.mean(scores):.4f}')






