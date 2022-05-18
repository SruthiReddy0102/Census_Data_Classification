#Performing Decision-Tree and Logistic Regression Comparison Model using Census Income Data

#Importing the Required Packages
library(tidyverse)  # data manipulation
library(caret)      # rocr analysis
library(ROCR)       # rocr analysis
library(gridExtra)  # arranging ggplot in grid
library(rpart)      # decision tree
library(rpart.plot) # decision tree plotting
library(caTools)    # (rolling, running) window statistic functions
library(ggplot2)    # create complex plots from data in a data frame
library(C50)        # Generate decision Tree
library(dplyr)      # Used for data manipulation challenges
library(plyr)       # split data apart, do stuff to it, and mash it back together.
library(gmodels)    # Various R programming tools for model fitting.
library(class)      # Print a Vector of class names
library(tree)       # a type of supervised learning algorithm   

#Loading the dataset
census_data = read.csv("F:/Salford Uni/CourseWork/ASDM/Task 1/CensusData.csv")

############### Data Pre Processing ##########################

# Converting categorical variables to Factors
census_data$Income <- as.factor(census_data$Income)
census_data$workclass <- as.factor(census_data$workclass)
census_data$marital.status <- as.factor(census_data$marital.status)
census_data$occupation <- as.factor(census_data$occupation)
census_data$relationship <- as.factor(census_data$relationship)
census_data$race <- as.factor(census_data$race)
census_data$sex <- as.factor(census_data$sex)

# Replacing ' ? ' with NA in Data-set
census_data[census_data == ' ?'] = NA 

# Dropping missing values of "workclass","Occupation" and native country as it consists only 1% of data
census_data <- na.omit(census_data)

# Removing insignificant columns based on the EDA 
census_data <- census_data[, -c(3,4,11,12,14)]
View(census_data)

#Reordering the variables
census_data <- census_data[,c(10, 1:9)]
attach(census_data)

# Splitting the Data into Train and Test
set.seed(1)
train <- sample(1:length(Income), length(Income)*0.7)
census.train <- census_data[train,]
census.test <- census_data[-train,]
str(census_data)

############## Decision Tree ########################
# check the proportion of class variable
prop.table(table(census.train$Income))
prop.table(table(census.test$Income))

# Training a model on the data
Census_model <- C5.0(census.train[, -1], census.train$Income)

# Display detailed information about the tree
summary(Census_model)

# Evaluating model performance
test_res <- predict(Census_model, census.test)
test_acc <- mean(census.test$Income == test_res)
test_acc

# cross tabulation of predicted versus actual classes
CrossTable(census.test$Income, test_res, dnn = c('actual Income', 'predicted Income'))

# On Training Data-set
train_res <- predict(Census_model, census.train)
train_acc <- mean(census.train$Income == train_res)
train_acc

# cross tabulation of predicted versus actual classes
CrossTable(census.train$Income, train_res, dnn = c('actual Income', 'predicted Income'))

################## Classification Tree ###############################

tree.income <- tree(Income~., data=census.test)
tree.pred <- predict(tree.income, newdata=census.test, type="class")
summary(tree.income)

plot(tree.income)
text(tree.income, pretty=0)

tree.table <- table(tree.pred, census.test$Income)
tree.acc <- mean(tree.pred == census.test$Income)
tree.acc

###################Logistic Regression ########################

model <- glm(Income ~ ., data = census_data, family = "binomial")
summary(model)

# Checking Prediction on model validation
prob <- predict(model, census_data, type = "response")
prob

# Train the model using Training data
model_train <- glm(Income ~ ., data = census.train, family = "binomial")
summary(model_train)

# Prediction on test data
prob_test <- predict(model, newdata = census.test, type = "response")
prob_test

# Confusion matrix 
confusion <- table(prob_test > 0.5, census.test$Income)
confusion

# Model Accuracy on Test Data
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy 

# Compare the model performance on Train data
# Prediction on test data
prob_train <- predict(model, newdata = census.train, type = "response")
prob_train

# Confusion matrix 
confusion_train <- table(prob_train > 0.5, census.train$Income)
confusion_train

# Model Accuracy on Train Data
Acc_train <- sum(diag(confusion_train)/sum(confusion_train))
Acc_train 


# ROC Curve is used to evaluate the betterness of the logistic model the more the area under ROC curve,the better is the model 
# We will not only use ROC curve for logistic regression but it can apply for any classification techniques.

rocrpred <- prediction(prob, census_data$Income)
rocrperf <- performance(rocrpred, 'tpr', 'fpr')
str(rocrperf)
plot(rocrperf, colorize=T, text.adj=c(-0.2,1.7) , main = "ROC Curve for Census Income")

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]], fpr=rocrperf@x.values, tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off, 6)

########################### KNN Model ########################
census.model <- model.matrix(Income~.-1,census_data)
model.train <- census.model[train,]
model.test <- census.model[-train,]
income.train <- Income[train]
income.test <- Income[-train]
set.seed(1)

knn.fit1 <- knn(model.train, model.test, income.train, k = 5)
knn.table1 <- table(knn.fit1,income.test)
knn.acc1 <- mean(knn.fit1==census.test$Income)
knn.acc1

knn.fit2 <- knn(model.train, model.test, income.train, k = 10)
knn.table2 <- table(knn.fit2,income.test)
knn.acc2 <- mean(knn.fit2==census.test$Income)
knn.acc2

knn.fit3 <- knn(model.train, model.test, income.train, k = 15)
knn.table3 <- table(knn.fit3,income.test)
knn.acc3 <- mean(knn.fit3==census.test$Income)
knn.acc3

knn.acc.all <- cbind(knn.acc1, knn.acc2, knn.acc3)
knn.acc <- knn.acc.all[which.max(knn.acc.all)]

# Graphical Representation of Results
all.acc <- cbind(test_acc, tree.acc, Accuracy, knn.acc1, knn.acc2, knn.acc3)
barplot(all.acc, col="mediumturquoise", main="Preliminary Results")

