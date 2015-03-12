library(randomForest)
data = read.csv('letter-recognition.data', header=FALSE, sep=',')
K = 5
for (sampleSize in seq(from=2000, to=16000, by=2000))
{
	epsilon = 0.005
	if(sampleSize <= 4000)
		epsilon = 0.015
	sampledData = data[sample(sampleSize),]
	N = nrow(sampledData)
	foldWidth = floor(N/K)
	print(sprintf("Sample Size: %d",sampleSize))
	for (i in (1:K))
	{	
		RTree = 500
		LTree = 2
		MTree = 250
		error = 0.5 
		start = as.integer((i-1)*foldWidth)+1
		end = as.integer(i*foldWidth)
		testData = sampledData[c(start:end),]
		learnData = sampledData[c(-start:-end),]
		forest = randomForest (
				learnData$V1~.,
				data=learnData,
				ntree=RTree, # 500
				replace = TRUE,
				nodesize=1,
				mtry=5
			)
		accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
		bestError = 1-accuracy
		print(sprintf("Error at %d Trees: %f",RTree,bestError))

		while(RTree - LTree > 1)
		{
			MTree = floor((RTree+LTree)/2)
			forest = randomForest (
				learnData$V1~.,
				data=learnData,
				ntree=MTree,
				replace = TRUE,
				nodesize=1,
				mtry=5
			)
			accuracy = sum(testData$V1==predict(forest, testData)) / nrow(testData)
			error = 1-accuracy
			if(error-bestError<epsilon)
				RTree = MTree
			else
				LTree = MTree
			print(sprintf("Error at %d Trees: %f",MTree,error))
		}
	}
}