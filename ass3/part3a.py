from scipy.stats import multivariate_normal as mvn
import numpy as np
import time
from sklearn.cluster import KMeans
def drange(start, stop, step):
	r = start
	while r < stop:
		yield r
		r += step

def classifier(p1, p2, p3, mu1, mu2, mu3, sigma, data):
	# p1 : weights of class1's 3 components
	# mu1 : means of class1's 3 components
	# sigma: common cov matrix
	# data: array of rows (21 cols)
	ans = []
	for row in data:
		prob1 = p1[0] * mvn.pdf(row, mu1[0], sigma) + p1[1] * mvn.pdf(row, mu1[1], sigma) + p1[2] * mvn.pdf(row, mu1[2], sigma)
		prob2 = p2[0] * mvn.pdf(row, mu2[0], sigma) + p2[1] * mvn.pdf(row, mu2[1], sigma) + p2[2] * mvn.pdf(row, mu2[2], sigma)
		prob3 = p3[0] * mvn.pdf(row, mu3[0], sigma) + p3[1] * mvn.pdf(row, mu3[1], sigma) + p3[2] * mvn.pdf(row, mu3[2], sigma)	
		# print prob1, prob2, prob3
		if(prob1>prob2 and prob1>prob3):
			ans.append(1)
		elif(prob2>prob3 and prob2>prob1):
			ans.append(2)
		else:
			ans.append(3)
	return ans

if __name__ == "__main__":
	startTime = time.time()
	data = np.genfromtxt('train.data', delimiter=' ')
	X = []
	y = []
	X1 = []
	X2 = []
	X3 = []
	for row in data:
		X.append(row[0:21])
		y.append(int(row[21]))
		if(row[21]==1):
			X1.append(row[0:21])
		elif(row[21]==2):
			X2.append(row[0:21])
		else:
			X3.append(row[0:21])

	X = np.array(X)
	X1 = np.array(X1)
	X2 = np.array(X2)
	X3 = np.array(X3)
	y = np.array(y)

	validatedata = np.genfromtxt('validate.data', delimiter=' ')
	validateX = []
	validatey = []
	for row in validatedata:
		validateX.append(row[0:21])
		validatey.append(int(row[21]))
	validateX = np.array(validateX)
	validatey = np.array(validatey)

	sigma = np.cov(X.T)
	# This sigma will stay constant.
	V = []
	for i in range(0,len(sigma)):
		V.append(sigma[i][i])
	V = np.array(V)
	p1 = [0.333,0.333,0.334]
	p2 = [0.333,0.333,0.334]
	p3 = [0.333,0.333,0.334]
	# weights of Gaussians. Need to search for best split. Should sum to 1.0
	# p1 corresponds to class1

	sampleMean1 = []
	sampleMean2 = []
	sampleMean3 = []
	# mu1 corresponds to class1. Has 3 vectors of means. One for each component of Gaussian

	##### Normal Sample Mean #####
	for i in range(0,3):
		sampleMean1.append(np.mean(X1, axis=0, dtype=np.float64))
		sampleMean2.append(np.mean(X2, axis=0, dtype=np.float64))
		sampleMean3.append(np.mean(X3, axis=0, dtype=np.float64))
	##############################
	
	# ### K - Means ##(not improving accuracy)###
	# model = KMeans(3)							#	
	# model.fit(X1)								#	
	# sampleMean1 = model.cluster_centers_		#	
	# model.fit(X2)								#				
	# sampleMean2 = model.cluster_centers_		#				
	# model.fit(X3)								#			
	# sampleMean3 = model.cluster_centers_		#				
	# ###########################################

	mu1 = sampleMean1
	mu2 = sampleMean2
	mu3 = sampleMean3
	bestp1 = p1
	bestp2 = p2
	bestp3 = p3
	bestMu1 = mu1
	bestMu2 = mu2
	bestMu3 = mu3
	bestSigma = sigma
	leastError = 1.0

	# This configuration will take close to 3 minutes.
	# make pstop and cstop little more(+0.01) to increase iterations. (~ 60 minutes)
	# |V| = 20 takes 44 sec per 1000 iter. |V| = 50 takes 110 sec per 1000 iter.
	stepSize = 0.2
	cStart = -0.4
	cStop = 0.4
	pStart = 0.2
	pStop = 0.6
	# class 1
	iterCount = 0
	for p1[0] in drange(pStart, pStop, stepSize):
		for p1[1] in drange(pStart, pStop, stepSize):
			p1[2] = 1.0 - p1[0] - p1[1]
			if(p1[2]<0.0):
				continue
			for c1 in drange(cStart, cStop, stepSize):
				for i1 in range(0,3):
					mu1[i1] = sampleMean1[i1] + c1*V

				# class 2
				for p2[0] in drange(pStart, pStop, stepSize):
					for p2[1] in drange(pStart, pStop, stepSize):
						p2[2] = 1.0 - p2[0] - p2[1]
						if(p2[2]<0.0):
							continue
						for c2 in drange(cStart, cStop, stepSize):
							for i2 in range(0,3):
								mu2[i2] = sampleMean2[i2] + c2*V

							# class 3
							for p3[0] in drange(pStart, pStop, stepSize):
								for p3[1] in drange(pStart, pStop, stepSize):
									p3[2] = 1.0 - p3[0] - p3[1]
									if(p3[2]<0.0):
										continue
									for c3 in drange(cStart, cStop, stepSize):
										for i3 in range(0,3):
											mu3[i3] = sampleMean3[i3] + c3*V
										
										predicted = classifier(p1, p2, p3, mu1, mu2, mu3, sigma, validateX)
										error = (1.0*np.sum(predicted != validatey)/len(validatey))
										iterCount+=1
										# if(iterCount%100==0):
										# 	print iterCount, error, time.time()-startTime
										if(error < leastError):
											# print iterCount, error, time.time()-startTime
											leastError = error
											bestp1 = p1[:]
											bestp2 = p2[:]
											bestp3 = p3[:]
											bestMu1 = mu1[:]
											bestMu2 = mu2[:]
											bestMu3 = mu3[:]

	testdata = np.genfromtxt('test.data', delimiter=' ')
	testX = []
	testy = []
	for row in testdata:
		testX.append(row[0:21])
		testy.append(int(row[21]))
	testX = np.array(testX)
	testy = np.array(testy)
	predicted = classifier(bestp1, bestp2, bestp3, bestMu1, bestMu2, bestMu3, sigma, testX)
	print "Test Set Error: ",
	print (1.0*np.sum(predicted != testy)/len(testy))
	endTime = time.time()
	print "Complete Execution in "+str(endTime-startTime)+" seconds"