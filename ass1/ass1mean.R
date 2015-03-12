library(rpart)
library(ROCR)
set.seed(10)
rawData = read.csv(file="data", header=F, sep=",")
originalData = rawData[sample(nrow(rawData)),]
colnames(originalData) = c("PregnantCount","Glucose","BP","Triceps","Insulin","BMI","DPF","Age","Class")
N = nrow(originalData)
K = 5
foldWidth = floor(N/K)
# Missing Values can be in Glucose, BP, Triceps, Insulin, BMI
Accuracy = 0
for (i in (1:K))
{	
	data = originalData
	start = as.integer((i-1)*foldWidth)+1
	end = as.integer(i*foldWidth)
	if(i==K)
	{
		end = N
	}
	testData = data[c(start:end),]
	learnData = data[c(-start:-end),]

	nonZerosCount = colSums(learnData!=0)		
	meanVals = colSums(learnData)/nonZerosCount
	learnData$Glucose[learnData$Glucose==0] = meanVals["Glucose"]	
	learnData$BP[learnData$BP==0] = meanVals["BP"]
	learnData$Triceps[learnData$Triceps==0] = meanVals["Triceps"]	
	learnData$Insulin[learnData$Insulin==0] = meanVals["Insulin"]
	learnData$BMI[learnData$BMI==0] = meanVals["BMI"]

	testData$Glucose[testData$Glucose==0] = NA 			
	testData$BP[testData$BP==0] = NA
	testData$Triceps[testData$Triceps==0] = NA
	testData$Insulin[testData$Insulin==0] = NA
	testData$BMI[testData$BMI==0] = NA
	diabStat = factor(learnData$Class, levels=0:1, labels=c('ND','D'))
	cfit = rpart(
					diabStat ~ PregnantCount+Glucose+BP+Triceps+Insulin+BMI+DPF+Age, 
					data = learnData, 
					na.action = na.rpart, 		# na.omit, try others too										# Missing Data
					method ='class',			# classification
					parms = list(split = "information"),	# "gini" and "information" (entropy)	# (b)
					control = rpart.control(
												cp = 0.01, 	# Threshold complexity parameter
												minsplit = 20, 	# Min no. of obs. for which the routine will even try to split 
												minbucket = 7,	# Min no. of obs in leaf. Default = minsplit/3
												# maxsurrogate = 10,  # max no. of surrogate vars to retain at each node
												usesurrogate = 2, 						# ?
												# xval = 5,								# CV ?
	              								surrogatestyle = 0, 					# ?
	              								maxdepth = 30,							# Threshold?
											)
				)
	opt = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"CP"]							# Pruning
	prunedTree = prune(cfit, cp = opt)
	# prunedTree = cfit
	predictedFactor = predict(prunedTree, testData, type="class")
	predictedFrame = as.data.frame.factor(predictedFactor)
	predicted = c(predictedFrame[ ,1]) - 1 									# WHY 1&2 :'( HELP!! 
	actual = testData$Class
	TP = sum(predicted & actual)
	TN = nrow(testData) - sum(predicted | actual)
	print("Accuracy: ")
	print((TP+TN)/nrow(testData))
	Accuracy = Accuracy + (TP+TN)/nrow(testData)

	metrics = prediction(predicted, actual)
	fname = sprintf("perf_fullgrowinfo%d.png",i)
	png(file=fname, width=800, height=600, res=90)
	plot(performance(metrics,"acc"))
	print("-----------")
	fname = sprintf("rplotpruneinfo_fullgrow%d.png",i)
	png(file=fname, width=800, height=600, res=90)
	plot(prunedTree, uniform=TRUE , main="Diabetes Decision Tree")
	text(prunedTree, use.n=TRUE, all=TRUE, cex=.7)
	# print(cfit)
	# summary(cfit)
}
print("Mean Accuracy:")
print(Accuracy/5)

