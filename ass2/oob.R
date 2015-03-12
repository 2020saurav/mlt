library(randomForest)
data = read.csv('letter-recognition.data', header=FALSE, sep=',')

numTree = 41

forest = randomForest (
	data$V1~.,
	data=data,
	ntree=numTree, 
	replace = TRUE,
	nodesize=1,
	mtry=5
)
oobError = (forest$err.rate[[numTree,1]])
print(oobError)

# 41 : 0.04675
# 500 : 0.03095