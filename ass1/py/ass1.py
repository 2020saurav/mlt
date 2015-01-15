from sklearn import tree
from subprocess import call
def num(s):
	try:
		return int(s)
	except ValueError:
		return float(s)
LX = []
LY = []
f = open('data')
lines = f.readlines()
for line in lines:
	values = line.rstrip('\n').split(",")
	values = [num(i) for i in values]
	LX.append(values[0:8])
	LY.append(values[8])
dtree = tree.DecisionTreeClassifier(criterion='entropy')
dtree = dtree.fit(LX,LY)
tree.export_graphviz(dtree,out_file='dtree.dot')
print dtree.predict([1, 200, 88, 3, 11, 1, 0.057, 30])
