#cleaning data enviroment and options setting
rm(list=ls())
options(scipen = 999)


#loading the datasets
setwd("C:/Users/Omer Gralnik/Desktop/Business Analysis/Final Project/Datasets")
list.files()
f1 <- read.csv("fraud1.csv", header = T )
f2 <- read.csv("fraud2.csv", header = T )


#generating the combined dataset and removing the ID variable
mydata <- rbind(f1,f2)
mydata <- mydata[,-1]

#removing used datasets
rm(f1)
rm(f2)


#checking the data's structure & summary
summary(mydata)
str(mydata)

#converting the variable into lower cases
names(mydata) <- tolower(names(mydata))


#creating train and test datasets
set.seed(10)
samplesize <- floor(0.7*(nrow(mydata)))
train.index <- sample(nrow(mydata), size = samplesize, replace = F)
train <- mydata[train.index,]
test <- mydata[-train.index,]


#loading the relevant libraries
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret) 
library("e1071")
library(class)



##############################DECISION TREE############################################



#model estimation
decision_tree <- rpart(class~., data = train, method = 'class' ,
                       parms = list(split = "information"),
                       control = rpart.control(cp = 0.001))
fancyRpartPlot(decision_tree)

#generating predictions
test$treepred <-  predict(decision_tree, newdata = test , type = 'class')

#checking accuracy
accuracy_tree <- mean(test$treepred==test$class)

#confusion matrix for the tree model
confusionMatrix(data = test$treepred , reference = as.factor(test$class))





################################LOGIT MODEL#############################################




#model estimation
logit_model <-  glm(class~., data = train, family = "binomial")

#checking the summary of the model
summary(logit_model)

#generating the prediction with decision boundary of 0.6
test$logitpredict <-  predict(logit_model, newdata = test, type = "response")
test$logitPred <-  ifelse(test$logitpredict>=0.6,1,0)
Accuracy_logit <-  mean(test$logitPred == test$class, na.rm = T)
test$logitpred <- NULL #removing unused variables
test$logitpredict <- NULL #removing unused variables

#confusion matrix for logit model
confusionMatrix(data = as.factor(test$logitPred) , reference = as.factor(test$class))



###################################KNN MODEL###########################################33



#normalizing the relevant variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
test_norm <- as.data.frame(lapply(test[1:30], normalize))
test_norm <- cbind(test_norm,test$class) #adding the class variable
train_norm <- as.data.frame(lapply(train[1:30], normalize))
train_norm <- cbind(train_norm,train$class) #adding the class variable

#fixing the names of the class variable
names(train_norm)[names(train_norm) == "train$class"] <- "class"
names(test_norm)[names(test_norm) == "test$class"] <- "class"

#estimating the model
knn_pred <-  knn(train_norm[,-31], test_norm[,-31],
                 cl = train_norm$class , k=10, prob = T)

#adding the predictions
test$knnpred <- knn_pred
summary(test)

#checking accuracy
Accuracy_knn <-  mean(test$knnpred == test$class, na.rm = T)

#confusion matrix for knn model
confusionMatrix(data = test$knnpred , reference = as.factor(test$class))





