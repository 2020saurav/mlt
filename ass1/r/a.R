library(rpart)
data = read.csv(file="data", header=F, sep=",")
colnames(data) = c("PregnantCount","Glucose","BP","Triceps","Insulin","BMI","DPF","Age","Class")
nonZerosCount = colSums(data!=0)
meanVals = colSums(data)/nonZerosCount
# Handle absurd/missing values
# Missing Values can be in Glucose, BP, Triceps, Insulin, BMI
data$Glucose[data$Glucose==0] = meanVals["Glucose"]
data$BP[data$BP==0] = meanVals["BP"]
data$Triceps[data$Triceps==0] = meanVals["Triceps"]
data$Insulin[data$Insulin==0] = meanVals["Insulin"]
data$BMI[data$BMI==0] = meanVals["BMI"]
data$DPF[data$DPF==0] = meanVals["DPF"]
data$Age[data$Age==0] = meanVals["Age"]
nonZerosCount = colSums(data!=0)
print(data)
# Threshold on decrease of impurity
# Threshold on number of data vectors at a node
# Grow the tree, then prune.
# Experiment with atleast two different impurity functions
diabStat = factor(data$Class, levels=0:1, labels=c('Non Diabetic','Diabetic'))
cfit = rpart(diabStat ~ PregnantCount+Glucose+BP+Triceps+Insulin+BMI+DPF+Age, data=data, method='class')
# print(cfit)
