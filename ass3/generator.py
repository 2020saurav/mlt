from random import *

noOfVectors = 5000 # 5000 for train, 500 or 1000 for test, 20 for validation 

def h1(i):
	return max(6-abs(i-11), 0)
def h2(i):
	return h1(i-4)
def h3(i):
	return h1(i+4)

for i in range(0, noOfVectors):
	classType = randint(1, 3)
	u = uniform(0.0, 1.0)
	for j in range(0, 21):
		eps = gauss(0.0, 1.0)
		if(classType==1):
			print format(u*h1(j) + (1.0-u)*h2(j) + eps, '.3f'),
		elif(classType==2):
			print format(u*h1(j) + (1.0-u)*h3(j) + eps, '.3f'),
		else:
			print format(u*h2(j) + (1.0-u)*h3(j) + eps, '.3f'),
	print classType