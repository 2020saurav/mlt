library('e1071')
library('rpart')
set.seed(10)
rawData = read.csv(file="spambase.data", header=F, sep=",")
originalData = rawData[sample(nrow(rawData)),]
N = nrow(originalData)
K = 5
Accuracy = 0
foldWidth = floor(N/K)
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
	model = svm(learnData$V58 ~ ., data=learnData, kernel='linear', type='C-classification')
	predicted = predict(model, testData[,-58])
	Accuracy = Accuracy + sum(testData$V58==predicted) / nrow(testData)
}
print(Accuracy/5)