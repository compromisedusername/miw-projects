import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_moons
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.metrics import accuracy_score, confusion_matrix, precision_score
from sklearn.metrics import recall_score


def gini_impurity(probs):
    return 1 - np.sum(np.square(probs))


def entropy(probs):
    return -np.sum(probs*np.log2(probs, where=(probs > 0)))

n_samples = 10000
test_size = 0.2

print("Dataset size: ", n_samples)
print("Trainging size: ", n_samples * (1-test_size))
print("Test size: ", n_samples * test_size )
# 1 Make moons dataset


X, y = make_moons(n_samples=n_samples, noise=0.4)

# 2 Spli dataset
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size)

# 3 Decision tree classifier
print("BEGINNING----------TREE-----------\n")
depths = [i*2 for i in range(1, 20)]
criteria = ["gini", "entropy"]


tree_results = {}


for criterion in criteria:
    tree_results[criterion] = []
    for depth in depths:
        tree_clf = DecisionTreeClassifier(max_depth=depth, criterion=criterion)
        tree_clf.fit(X_train, y_train)
        y_pred = tree_clf.predict(X_test)
        accuracy = accuracy_score(y_test, y_pred)
        tree_results[criterion].append((depth, accuracy, y_pred))


max_entropy = max(tree_results["entropy"], key=lambda x: x[1])
max_gini = max(tree_results["gini"], key=lambda x: x[1])

cm_entropy = confusion_matrix(y_test, max_entropy[2])
cm_gini = confusion_matrix(y_test, max_gini[2])

entropy_precision = precision_score(y_test, max_entropy[2])
gini_precision = precision_score(y_test, max_gini[2])
entropy_recall = recall_score(y_test, max_entropy[2])
gini_recall = recall_score(y_test, max_entropy[2])
print(tree_results)
print(f"Gini max ACCURACY (depth: {max_gini[0]}) : {max_gini[1]}")
print(f"Gini max CONFUSION MATRIX:\n", cm_gini)
print(f"Gini max PRECISION", gini_precision)
print(f"Gini max RECALL :\n", gini_recall)
print(f"Entropy max ACCURACY (depth: {max_entropy[0]}): {max_entropy[1]}")
print(f"Entropy max CONFUSION MATRIX:\n", cm_entropy)
print(f"Entropy max PRECISION", entropy_precision)
print(f"Entropy max RECALL :\n", entropy_recall)
print("END----------------TREE-----------\n")

# 4 Random forest
print("BEGINNING----------FOREST-----------\n")
n_estimators_list = [i*5 for i in range(1, 5)]

forest_results = {}
for n_estimators in n_estimators_list:
    forest_clf = RandomForestClassifier(n_estimators=n_estimators)
    forest_clf.fit(X_train, y_train)
    y_pred = forest_clf.predict(X_test)
    acc = accuracy_score(y_test, y_pred)
    forest_results[n_estimators] = (acc, y_pred)


max_forest, (max_forest_acc, max_forest_pred) = max(forest_results.items(), key=lambda x: x[1][0])

cm_forest = confusion_matrix(y_test, max_forest_pred)
forest_precision = precision_score(y_test, max_forest_pred)
forest_recall = recall_score(y_test, max_forest_pred)

print("Forest results ACCURACY:", forest_results)
print(f"Forest results CONFUSION MATRIX:\n{cm_forest}")
print(f"Forest results PRECISION: {forest_precision}")
print(f"Forest results RECALL: {forest_recall}")
print("END----------------FOREST-----------\n")

# 5 Logistic Regression and SVM

print("BEGINNING----------LOGISTIC REGRESSION AND SVM-----------\n")
log_reg_clf = LogisticRegression()
log_reg_clf.fit(X_train, y_train)
y_pred_log = log_reg_clf.predict(X_test)

acc_log = accuracy_score(y_test, y_pred_log)
cm_log_reg = confusion_matrix(y_test, y_pred_log)
log_reg_precision = precision_score(y_test, y_pred_log)
log_reg_recall = recall_score(y_test, y_pred_log)

print("Logistic Regression ACCURACY:", acc_log)
print(f"Logistic Regression CONFUSION MATRIX:\n{cm_log_reg}")
print(f"Logistic Regression PRECISION: {log_reg_precision}")
print(f"Logistic Regression RECALL: {log_reg_recall}")


svm_clf = SVC(probability=True)
svm_clf.fit(X_train, y_train)
y_pred_svm = svm_clf.predict(X_test)
acc_svm = accuracy_score(y_test, y_pred_svm)
cm_svm = confusion_matrix(y_test, y_pred_svm)
svm_precision = precision_score(y_test, y_pred_svm)
svm_recall = recall_score(y_test, y_pred_svm)

print("Support Vector Machine ACCURACY: ", acc_svm)
print(f"Support Vector Machine CONFUSION MATRIX:\n{cm_svm}")
print(f"Support Vector Machine PRECISION: {svm_precision}")
print(f"Support Vector Machine RECALL: {svm_recall}")
print("END----------------LOGISTIC REGRESSION AND SVM-----------\n")

print("BEGINNING----------VOTING CLASSIFIERS-----------\n")
voting_clf = VotingClassifier(estimators=[(
    'lr', log_reg_clf), ('svm', svm_clf), ('rf', forest_clf)], voting="soft")
voting_clf.fit(X_train, y_train)
y_pred_voting = voting_clf.predict(X_test)
acc_voting = accuracy_score(y_test, y_pred_voting)
cm_voting = confusion_matrix(y_test, y_pred_voting)
voting_precision = precision_score(y_test, y_pred_voting)
voting_recall = recall_score(y_test, y_pred_voting)
print("Voting Classifier ACCURACY: ", acc_voting)
print(f"Voting Classifier CONFUSION MATRIX:\n{cm_voting}")
print(f"Voting Classifier PRECISION: {voting_precision}")
print(f"Voting Classifier RECALL: {voting_recall}")

print("END----------------VOTING CLASSIFIERS-----------\n")
