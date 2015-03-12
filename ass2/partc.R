library(randomForest)
data = read.csv('letter-recognition.data', header=FALSE, sep=',')
K = 5
N = nrow(data)
foldWidth = floor(N/K)
m = 8
treeCount = 52 # 1.25 times
for (i in (1:K))
{
	start = as.integer((i-1)*foldWidth)+1
	end = as.integer(i*foldWidth)

	testData = data[c(start:end),]
	learnData = data[c(-start:-end),]
	
	forest = randomForest (
		learnData$V1~.,
		data=learnData,
		ntree=treeCount,
		replace = TRUE,
		nodesize=1,
		mtry=m
	)
	accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
	error = 1-accuracy
	print(sprintf("%d fold : %d Trees : %f Error",i,treeCount,error))
}