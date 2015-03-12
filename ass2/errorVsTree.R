library(randomForest)
data = read.csv('letter-recognition.data', header=FALSE, sep=',')
data = data[sample(2000),]
K = 5
N = nrow(data)
foldWidth = floor(N/K)
for (i in (1:K))
{
	start = as.integer((i-1)*foldWidth)+1
	end = as.integer(i*foldWidth)

	testData = data[c(start:end),]
	learnData = data[c(-start:-end),]
	for(treeCount in seq(from=2, to=20, by=1))
	{
		forest = randomForest (
			learnData$V1~.,
			data=learnData,
			ntree=treeCount,
			replace = TRUE,
			nodesize=1,
			mtry=5
		)
		accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
		error = 1-accuracy
		print(sprintf("%d fold : %d Trees : %f Error",i,treeCount,error))
	}
	for(treeCount in seq(from=25, to=526, by=100))
	{
		forest = randomForest (
			learnData$V1~.,
			data=learnData,
			ntree=treeCount,
			replace = TRUE,
			nodesize=1,
			mtry=5
		)
		accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
		error = 1-accuracy
		print(sprintf("%d fold : %d Trees : %f Error",i,treeCount,error))
	}
}