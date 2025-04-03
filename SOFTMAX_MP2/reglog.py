# Importujemy potrzebne biblioteki
import numpy as np                      # obliczenia matematyczne
import matplotlib.pylab as plt          # rysowanie wykresów
from sklearn import datasets            # zbiór danych o kwiatach
from sklearn.model_selection import train_test_split   # podział danych na trening/test
from plotka import plot_decision_regions # funkcja do rysowania granic decyzyjnych (twój plik)

# Implementacja regresji logistycznej z gradient descent (uczenie wag)
class LogisticRegressionGD(object):
    def __init__(self, eta=0.05, n_iter=100, random_state=1):
        self.eta = eta                  # learning rate - jak duże kroki robimy
        self.n_iter = n_iter            # ile razy przechodzimy przez dane (epoki)
        self.random_state = random_state # seed dla losowości (żeby wyniki były powtarzalne)

    def fit(self, X, y):
        # Losowanie początkowych wag (małe wartości bliskie 0)
        rgen = np.random.RandomState(self.random_state)
        self.w_ = rgen.normal(loc=0.0, scale=0.01, size=1 + X.shape[1])  # wagi + bias
        print()
        print("X shape" , X.shape[1])
        print("SELF W: ",self.w_)

        # Uczenie modelu
        for i in range(self.n_iter):
            net_input = self.net_input(X)     # oblicz sumę ważoną w każdej próbce
            output = self.activation(net_input) # aktywacja (sigmoid)
            errors = (y - output)             # oblicz błąd predykcji
            # aktualizacja wag
            self.w_[1:] += self.eta * X.T.dot(errors)  # update wag
            self.w_[0] += self.eta * errors.sum()      # update biasu
        return self

    def net_input(self, X):
        # Obliczenie sumy ważonej (w * x + b)
        return np.dot(X, self.w_[1:]) + self.w_[0]

    def activation(self, z):
        # Funkcja aktywacji - sigmoid
        return 1. / (1. + np.exp(-np.clip(z, -250, 250)))  # zabezpieczenie przed overflow

    def predict(self, X):
        # Predykcja klasy 0 lub 1
        return np.where(self.net_input(X) >= 0.0, 1, 0)


class SoftmaxOVRClassifier(object):
    def __init__(self, eta=0.05, n_iter=1000, random_state=1):
        self.eta = eta
        self.n_iter = n_iter
        self.random_state = random_state
        self.classifiers = []

    def fit(self, X, y):
        self.n_classes = np.unique(y).shape[0]
        self.classifiers = []

        for class_idx in range(self.n_classes):
            y_binary = np.where(y == class_idx, 1, 0)
            clf = LogisticRegressionGD(eta=self.eta, n_iter=self.n_iter)
            clf.fit(X, y_binary)
            self.classifiers.append(clf)

    def predict_proba(self, X):
        probs = np.array([clf.activation(clf.net_input(X)) for clf in self.classifiers ])
        norm_probs = probs / np.sum(probs, axis=0)
        return norm_probs.T

    def predict(self, X):
        norm_probs = self.predict_proba(X)
        return np.argmax(norm_probs, axis=1)

# ------------------------------
# Główna funkcja programu
# ------------------------------
def main():
    # Wczytanie danych o kwiatach (iris dataset)
    iris = datasets.load_iris()

    # Wybieramy tylko 2 cechy: długość i szerokość płatka (łatwiej narysować w 2D)
    X = iris.data[:, [2, 3]]
    y = iris.target

    print(y)

    # Podział danych na zbiór treningowy i testowy
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.9, random_state=1, stratify=y)

    model_softmax = SoftmaxOVRClassifier()
    model_softmax.fit(X_train, y_train)

    print("Prawdopodobieństwa: ", model_softmax.predict_proba(X_test))
    print("Klasy", model_softmax.predict(X_test))



    # Rysowanie granicy decyzyjnej
    plot_decision_regions(X=X_train, y=y_train, classifier=model_softmax)
    plt.xlabel(r'$x_1$')  # opis osi x
    plt.ylabel(r'$x_2$')  # opis osi y
    plt.legend(loc='upper left')
    plt.show()



# ------------------------------
# Start programu
# ------------------------------
if __name__ == '__main__':
    main()

