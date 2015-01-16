library(rpart)
library(ROCR)
data = read.csv(file="data", header=F, sep=",")
colnames(data) = c("PregnantCount","Glucose","BP","Triceps","Insulin","BMI","DPF","Age","Class")
N = nrow(data)
K = 5
foldWidth = floor(N/K)
# Missing Values can be in Glucose, BP, Triceps, Insulin, BMI
data$Glucose[data$Glucose==0] = NA 													# Missing Data
data$BP[data$BP==0] = NA
data$Triceps[data$Triceps==0] = NA
data$Insulin[data$Insulin==0] = NA
data$BMI[data$BMI==0] = NA

for (i in (1:K))
{	
	start = as.integer((i-1)*foldWidth)+1
	end = as.integer(i*foldWidth)
	testData = data[c(start:end),]
	learnData = data[c(-start:-end),]
	diabStat = factor(learnData$Class, levels=0:1, labels=c('ND','D'))
	cfit = rpart(
					diabStat ~ PregnantCount+Glucose+BP+Triceps+Insulin+BMI+DPF+Age, 
					data = learnData, 
					na.action = na.rpart, 												# Missing Data
					method ='class',
					parms = list(split = "gini"),										# Impurity Function : Gini/Information
					control = rpart.control(
												cp = 0.01, 								# Threshold?
												minsplit = 2, 							# Threshold?
												maxcompete = 4,							# ?
												maxsurrogate = 5, 						# ?
												usesurrogate = 2, 						# ?
												xval = 5,								# CV ?
	              								surrogatestyle = 0, 					# ?
	              								maxdepth = 30,							# Threshold?
											)
				)

	opt = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"CP"]							# Pruning
	prunedTree = prune(cfit, cp = opt)

	predictedFactor = predict(prunedTree, testData, type="class")
	predictedFrame = as.data.frame.factor(predictedFactor)
	predicted = c(predictedFrame[ ,1]) - 1 									# WHY 1&2 :'( HELP!! 
	actual = testData$Class
	# print(predicted)
	# print(actual)
	metrics = prediction(predicted, actual)
	fname = sprintf("perf%d.png",i)
	
	png(file=fname, width=800, height=600, res=90)
	plot(performance(metrics,"f"))
	print("-----------")

	# fname = sprintf("rplotprunegini%d_%d.png",i,as.integer(Sys.time()))
	# fname = sprintf("rplotprunegini%d.png",i)
	# png(file=fname, width=800, height=600, res=90)
	# plot(prunedTree, uniform=TRUE , main="Diabetes Decision Tree")
	# text(prunedTree, use.n=TRUE, all=TRUE, cex=.7)
	# print(cfit)
	# summary(cfit)
}

