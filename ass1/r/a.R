library(rpart)
data = read.csv(file="data", header=F, sep=",")
colnames(data) = c("PregnantCount","Glucose","BP","Triceps","Insulin","BMI","DPF","Age","Class")
# Handle absurd/missing values
# Missing Values can be in Glucose, BP, Triceps, Insulin, BMI
data$Glucose[data$Glucose==0] = NA
data$BP[data$BP==0] = NA
data$Triceps[data$Triceps==0] = NA
data$Insulin[data$Insulin==0] = NA
data$BMI[data$BMI==0] = NA
# Threshold on decrease of impurity
# Threshold on number of data vectors at a node
# Grow the tree, then prune.
# Experiment with atleast two different impurity functions
diabStat = factor(data$Class, levels=0:1, labels=c('ND','D'))
cfit = rpart(
				diabStat ~ PregnantCount+Glucose+BP+Triceps+Insulin+BMI+DPF+Age, 
				data = data, 
				na.action = na.rpart, 
				method ='class',
				control = rpart.control(
											# cp = 0.01,
											# minsplit = 2, # the minimum number of observations that must exist in a node in order for a split to be attempted.
											# maxcompete = 4,
											# maxsurrogate = 5, 
											# usesurrogate = 2, 
											xval = 5,
              								surrogatestyle = 0, 
              								# maxdepth = 30,
										)

			)
opt = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"CP"]
prunedTree = prune(cfit, cp = opt)
prunedTree = cfit
png(file="rplotprune1.png", width=800, height=600, res=90)
plot(prunedTree, uniform=TRUE , main="Diabetes Decision Tree")
text(prunedTree, use.n=TRUE, all=TRUE, cex=.7)
# print(cfit)
# summary(cfit)
# print(cfit$cptable)
