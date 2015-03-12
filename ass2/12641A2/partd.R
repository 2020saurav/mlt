library(randomForest)
data = read.csv('letter-recognition.data', header=FALSE, sep=',')
K = 5
N = nrow(data)
foldWidth = floor(N/K)
epsilon = 0.005

for (sampleSize in seq(from=0.8*2000, to=0.8*16000, by=0.8*2000)) # size of learning set is 0.8*|data|
{
	print(sprintf("Sample Size: %d",sampleSize))
	for(i in (1:K))
	{
		RTree = 500
		LTree = 2
		MTree = 250
		error = 0.5 
		start = as.integer((i-1)*foldWidth)+1
		end = as.integer(i*foldWidth)
		testData = data[c(start:end),]
		learnData = data[c(-start:-end),]
		forest = randomForest (
				learnData$V1~.,
				data=learnData,
				ntree=RTree, # 500
				replace = FALSE,
				sampsize = sampleSize,
				nodesize=1,
				mtry=5
			)
		accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
		bestError = 1-accuracy
		# print(sprintf("Error at %d Trees: %f",RTree,bestError))

		while(RTree - LTree > 1)
		{
			MTree = floor((RTree+LTree)/2)
			forest = randomForest (
				learnData$V1~.,
				data=learnData,
				ntree=MTree,
				replace = FALSE,
				sampsize = sampleSize,
				nodesize=1,
				mtry=5
			)
			accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
			error = 1-accuracy
			if(error-bestError<epsilon)
				RTree = MTree
			else
				LTree = MTree
			# print(sprintf("Error at %d Trees: %f",MTree,error))
		}
		print(sprintf("Error at %d Trees: %f",MTree,error))
	}
}