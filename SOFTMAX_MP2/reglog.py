import pandas as pd
import numpy as np
import matplotlib.pylab as plt
from sklearn import datasets
from sklearn.model_selection import train_test_split
from plotka import plot_decision_regions

pd.set_option("display.max_rows", None)


class LogisticRegressionGD(object):
    def __init__(self, eta=0.05, n_iter=100, random_state=1):
        self.eta = eta
        self.n_iter = n_iter
        self.random_state = random_state

    def fit(self, X, y):
        rgen = np.random.RandomState(self.random_state)
        self.w_ = rgen.normal(loc=0.0, scale=0.01, size=1 + X.shape[1])

        for i in range(self.n_iter):
            net_input = self.net_input(X)
            output = self.activation(net_input)
            errors = (y - output)
            self.w_[1:] += self.eta * X.T.dot(errors)
            self.w_[0] += self.eta * errors.sum()
        return self

    def net_input(self, X):
        return np.dot(X, self.w_[1:]) + self.w_[0]

    def activation(self, z):
        return 1. / (1. + np.exp(-np.clip(z, -250, 250)))

    def predict(self, X):
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
        probs = np.array([clf.activation(clf.net_input(X))
                         for clf in self.classifiers])
        norm_probs = probs / np.sum(probs, axis=0)
        return norm_probs.T

    def predict(self, X):
        norm_probs = self.predict_proba(X)
        return np.argmax(norm_probs, axis=1)


def main():
    iris = datasets.load_iris()

    X = iris.data[:, [2, 3]]
    y = iris.target

    print(y)

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.9, random_state=1, stratify=y)

    model_softmax = SoftmaxOVRClassifier()
    model_softmax.fit(X_train, y_train)

    classes = model_softmax.predict(X_test)
    probs = model_softmax.predict_proba(X_test)
    df = pd.DataFrame(
        probs, columns=[f"Class {i}" for i in range(probs.shape[1])])
    df = df.map(lambda x: f"{x*100:.2f}%")

    df["Predicted class"] = df.idxmax(axis=1).apply(lambda x: f"{x}")

    print("Normalized probabilites:\n", df)
    print("Classes: ", classes)

    plot_decision_regions(X=X_train, y=y_train, classifier=model_softmax)
    plt.xlabel(r'$x_1$')
    plt.ylabel(r'$x_2$')
    plt.legend(loc='upper left')
    plt.show()


if __name__ == '__main__':
    main()
