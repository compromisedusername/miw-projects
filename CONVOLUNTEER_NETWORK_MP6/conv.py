import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from keras.datasets import cifar10
from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense

def plot_history(history, model_name):
    plt.plot(history.history['accuracy'], label=f'{model_name} - Train Accuracy')
    plt.plot(history.history['val_accuracy'], label=f'{model_name} - Val Accuracy')

def map_labels(y):
    return np.where((y == 0) | (y == 1) | (y == 8) | (y == 9), 0, 1)  # 0 - animal, 1 - vehicle

def unpickle(file):
    import pickle
    with open(file, 'rb') as fo:
        dict = pickle.load(fo, encoding='bytes')
    return dict


labels = unpickle('./batches.meta')

print("Labels: ")
for idx, label in enumerate(labels[b'label_names']):
    print(f"{idx}: {label.decode('utf-8')}")

(x_train, y_train), (x_test, y_test) = cifar10.load_data()

y_train = map_labels(y_train)
y_test = map_labels(y_test)

x_train, x_val, y_train, y_val = train_test_split(x_train, y_train, test_size=0.7, random_state=42)

x_train = x_train.astype('float32') / 255.0
x_val = x_val.astype('float32') / 255.0
x_test = x_test.astype('float32') / 255.0


# Model 1 - 1 splot
model_1 = Sequential()
model_1.add(Conv2D(32, (3, 3), activation='relu', input_shape=(32, 32, 3)))
model_1.add(MaxPooling2D((2, 2)))
model_1.add(Flatten())
model_1.add(Dense(64, activation='relu'))
model_1.add(Dense(1, activation='sigmoid'))  # Binary classification

model_1.compile(optimizer='rmsprop', loss='binary_crossentropy', metrics=['accuracy'])
# Model 2 - 2 splot

# Model z dwiema warstwami splotowymi
model_2 = Sequential()
model_2.add(Conv2D(32, (3, 3), activation='relu', input_shape=(32, 32, 3)))
model_2.add(MaxPooling2D((2, 2)))
model_2.add(Conv2D(64, (3, 3), activation='relu'))
model_2.add(MaxPooling2D((2, 2)))
model_2.add(Flatten())
model_2.add(Dense(64, activation='relu'))
model_2.add(Dense(1, activation='sigmoid'))

model_2.compile(optimizer='rmsprop', loss='binary_crossentropy', metrics=['accuracy'])

# Model 3 - 3 splot
model_3 = Sequential()
model_3.add(Conv2D(32, (3, 3), activation='relu', input_shape=(32, 32, 3)))
model_3.add(MaxPooling2D((2, 2)))
model_3.add(Conv2D(64, (3, 3), activation='relu'))
model_3.add(MaxPooling2D((2, 2)))
model_3.add(Conv2D(128, (3, 3), activation='relu'))
model_3.add(MaxPooling2D((2, 2)))
model_3.add(Flatten())
model_3.add(Dense(64, activation='relu'))
model_3.add(Dense(1, activation='sigmoid'))

model_3.compile(optimizer='rmsprop', loss='binary_crossentropy', metrics=['accuracy'])

# Training model 1
history_1 = model_1.fit(x_train, y_train, epochs=3, batch_size=64, validation_data=(x_val, y_val), verbose=1)

# Training model 2
history_2 = model_2.fit(x_train, y_train, epochs=3, batch_size=64, validation_data=(x_val, y_val), verbose=1)

# Training model 3
history_3 = model_3.fit(x_train, y_train, epochs=3, batch_size=64, validation_data=(x_val, y_val), verbose=1)


plot_history(history_1, 'Model 1')
plot_history(history_2, 'Model 2')
plot_history(history_3, 'Model 3')

plt.title('Model comparision')
plt.xlabel('Epochs')
plt.ylabel('Accuracy')
plt.legend()
plt.show()
