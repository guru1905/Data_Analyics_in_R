###########################################################################################
SUPPORT VECTOR MACHINE(SVM)
###################################################################################
###################################################
#import datasetS
dataset=read.csv('Customers.csv')
View(dataset)
###################################

library(caTools)
library(ggplot2)
library(GGally)
library(e1071)

#data SPLIT
split=sample.split(dataset$Age,SplitRatio=0.75)

#training and testing

training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split== FALSE)
View(training_set)
View(testing_set)

#dataset plot

ggpairs(training_set,ggplot2::aes(colour=Gender,alpha=0.6))
#scaling data
training_set[,1:4]=scale(training_set[,1:8])
testing_set[,1:4]=scale(testing_set[,1:8])

#svc
classifier1 =svm(formula= Spending_Score~.,data=training_set,type='C-classification',kernel='radial')

#prediction
pred1=predict(classifier1,type='response',newdata=testing_set)

#Actual&prediction comparison

comp=data.frame(testing_set$Gender.pred1)
View(comp)

#confusion matrix
cm1=table(testing_set[,5],pred1)
cm1













##########################################################################
#MULTIPLE LINEAR REGRESSION
##########################################################################
# Slicing data
data = dataset[,1:8]

# Importing Data Splitting library
library(caTools)

# Generating random numbers
set.seed(123)

# Data Splitting
split=sample.split(data$Spending_Score,SplitRatio = 0.8)

# Training data
training_set=subset(data,split==TRUE)
View(training_set)

# Testing Data
testing_set=subset(data,split==FALSE)
View(testing_set)

# Applying Regression
regressor = lm(formula = Spending_Score~., data = training_set)

# Prediction
y_pred = predict(regressor, newdata = testing_set)

# Visualization
plot(testing_set$Spending_Score,type='o',col='blue',
     xlab = 'SPENDING_SCORE', ylab = 'AGE')
lines(y_pred,type='o',col='red')
#####################################################################################

#SINGLE LINEAR REGRESSION 
#################################################################################
# Slicing Data
Age = dataset[,4]
Spending_score = dataset[,5]

# Dataset for SLR
data = data.frame(Age,Spending_score)

# Importing Data Splitting library
library(caTools)

# Generating Random Numbers
set.seed(42)

# Data Splitting
split=sample.split(data$Spending_score,SplitRatio = 0.8)

# Training data
training_set=subset(data,split==TRUE)
View(training_set)

# Testing data
testing_set=subset(data,split==FALSE)
View(testing_set)

# Applying Regression
regressor = lm(formula = Spending_score~Age, data = training_set)

# Regression Summary
summary(regressor)

# Prediction
y_pred = predict(regressor, newdata = testing_set)

# Comparison
comp = data.frame(testing_set$Spending_score, y_pred)

# Visualization
plot(regressor,col='blue')


plot(testing_set$Age,testing_set$Spending_score,type='p',col='blue',
     xlab = 'AGE', ylab = 'Spending_score')
lines(testing_set$Age,y_pred,type='o',col='red')



# Importing Library for Performance Metrics
library(caret)

# Mean Absolute Error
MAE(y_pred, testing_set$coa)

# Root Mean Square Error
RMSE(y_pred, testing_set$coa)

# R-square Score
R2(y_pred, testing_set$coa)


