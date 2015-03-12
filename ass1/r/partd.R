library(randomForest)
data = read.csv('letter-recognition.data', header=FALSE, sep=',')
data = read.csv('letter-recognition.data', header=FALSE, sep=',')
# sampledData = data[sample(5),]
# print(sampledData)
# K = 5
# N = nrow(data)
# foldWidth = floor(N/K)
for (i in (1:K))
{	
	# start = as.integer((i-1)*foldWidth)+1
	# end = as.integer(i*foldWidth)

	# testData = data[c(start:end),]
	# learnData = data[c(-start:-end),]
	# forest = randomForest (
	# 	learnData$V1~.,
	# 	data=learnData,
	# 	ntree=20,
	# 	replace = TRUE,
	# 	nodesize=1,
	# 	mtry=5
	# )
	# # print(forest$err.rate)
	# accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
	# error = 1-accuracy
	# print(sprintf("Error in %dth fold %f",i,error))

	# # print("-----------")
	# # print(predict(forest, testData))
	# # print("-----------")
	# # print(forest$oob.times)
}