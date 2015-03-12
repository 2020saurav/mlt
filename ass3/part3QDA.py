import numpy as np
from sklearn.qda import QDA

data = np.genfromtxt('train.data', delimiter=' ')
X = []
y = []
for row in data:
	X.append(row[0:21])
	y.append(int(row[21]))
X = np.array(X)
y = np.array(y)

model = QDA()
model.fit(X, y)

testdata = np.genfromtxt('test.data', delimiter=' ')
testX = []
testy = []
for row in testdata:
	testX.append(row[0:21])
	testy.append(int(row[21]))
testX = np.array(testX)
testy = np.array(testy)

print "Error: ",
print (1.0*np.sum(model.predict(testX) != testy)/len(testy))