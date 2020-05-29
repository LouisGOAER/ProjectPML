
# Louis GOAER, EDHEC Student
# Coursera Project, Prediction assignment


###################################################################################
#                       TABLE OF CONTENTS OF THE PROGRAM :                        #
###################################################################################

# Part 0: PACKAGES
# Part 1: LOAD THE DATA
# Part 2: CLEAR THE DATA
# Part 3: CONVERT THE FACTORS INTO INTEGERS
# Part 4: BUILD A TRAIN SET AND A TEST SET FROM THE TRAINING DATA
# Part 5: BUILD MODELS AND PREDICT CLASSES OF THE TEST SET
# Part 6: COMPARISON BETWEEN THE MODELS' ACCURACY
# Part 7: PREDICTION OF THE TESTING DATA'S CLASSES


###################################################################################
#                                  PROGRAM :                                      #
###################################################################################


### Part 0: PACKAGES_______________________________________________________________

install.packages("gbm")
install.packages("caret")
install.packages("randomForest")
install.packages("dplyr")

library(gbm)
library(caret)
library(randomForest)
library(dplyr)


### PART 1: LOAD AND CLEAR THE DATA________________________________________________ 

# Clear the worplace
rm(list=ls())

# Load the datasets (/!\ INTERNET CONNECTION REQUIRED !)
training.data.raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",header=TRUE,sep=",")
testing.data.raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",header=TRUE,sep=",")

# Further information on the testing dataset:
dim(testing.data.raw)
str(testing.data.raw)
dim(training.data.raw)
str(training.data.raw)




### PART 2: CLEAR THE DATA_________________________________________________________

#NAs, empty and unsignificant columns cleared
training.data <- training.data.raw[,c(2,6:11,37:49,60:68,84:86,102,113:124,140,151:160)]
testing.data <- testing.data.raw[,c(2,6:11,37:49,60:68,84:86,102,113:124,140,151:160)]




### PART 3: CONVERT THE FACTORS INTO INTEGERS______________________________________

classeLevels <- levels(training.data$classe)
training.data <- data.frame(data.matrix(training.data)) 
training.data$classe <- factor(training.data$classe,labels=classeLevels)
testing.data <- data.frame(data.matrix(testing.data))




### PART 4: BUILD A TRAIN SET AND A TEST SET FROM THE TRAINING DATA________________

set.seed(2803198)

library(caret)

classeIndex <- which(names(training.data)=="classe")
inTrain <- createDataPartition(y=training.data$classe,p=0.75,list=FALSE)

train.set <- training.data[inTrain,]
test.set <- training.data[-inTrain,]




### PART 5: BUILD MODELS AND PREDICT CLASSES OF THE TEST SET_______________________

library(caret)

# Trees model
modFit.trees<-train(classe~.,method="rpart",data=train.set)
prediction.trees<-predict(modFit.trees,newdata=test.set)

# Random Forest model
modFit.rf<-randomForest(classe~.,data=train.set,ntree=250)
prediction.rf<-predict(modFit.rf,newdata=test.set)

# Boosting model (/!\ IT CAN TAKES SEVERAL MINUTES...)
modFit.boost<-train(classe~.,method="gbm",data = train.set,verbose=FALSE,trControl=trainControl(method="cv",5))
prediction.boost<-predict(modFit.boost,newdata=test.set)

# Bagging model (/!\ IT CAN TAKES SEVERAL MINUTES...)
predictors.bag<-train.set[,-classeIndex]
classe.bag<-train.set$classe
modFit.bag<-bag(predictors.bag,classe.bag,B=10,bagControl=bagControl(fit = ctreeBag$fit,predict = ctreeBag$pred,aggregate = ctreeBag$aggregate))
prediction.bag<-predict(modFit.bag,newdata = test.set)




### PART 6: COMPARISON BETWEEN THE MODELS' ACCURACY________________________________

library(dplyr)

# Trees model
confusion.matrix.trees<-table(prediction.trees,test.set$classe)
table.trees<-confusion.matrix.trees/length(test.set$classe)
accuracy.trees<-sum(diag(table.trees))

# Random Forest model
confusion.matrix.rf<-table(prediction.rf,test.set$classe)
table.rf<-confusion.matrix.rf/length(test.set$classe)
accuracy.rf<-sum(diag(table.rf))

# Boosting model
confusion.matrix.boost<-table(prediction.boost,test.set$classe)
table.boost<-confusion.matrix.boost/length(test.set$classe)
accuracy.boost<-sum(diag(table.boost))

# Bagging model
confusion.matrix.bag<-table(prediction.bag,test.set$classe)
table.bag<-confusion.matrix.bag/length(test.set$classe)
accuracy.bag<-sum(diag(table.bag))


# Confusion matrix and tables sum up
# Trees model
confusion.matrix.trees ; table.trees %>% round(digits = 3) ; c("Accuracy:" , round(accuracy.trees,digits = 5))
# Random Forest model
confusion.matrix.rf ; table.rf %>% round(digits = 3) ; c("Accuracy:" , round(accuracy.rf,digits = 5))
# Boosting model
confusion.matrix.boost ; table.boost %>% round(digits = 3) ; c("Accuracy:" , round(accuracy.boost,digits = 5))
# Bagging model
confusion.matrix.bag ; table.bag %>% round(digits = 3); c("Accuracy:" , round(accuracy.bag,digits = 5))


# Accuracy sum up
accuracy=round(as.numeric(c(accuracy.trees,accuracy.rf,accuracy.boost,accuracy.bag)),digits = 4)
sumUp.accuracy=data.frame(accuracy)
rownames(sumUp.accuracy)=c("Trees","random Forest","Boosting","Bagging")
sumUp.accuracy

# Error sum up
sumUp.error=1-sumUp.accuracy
colnames(sumUp.error)=c("error")
sumUp.error




### PART 7: PREDICTION OF THE TESTING DATA'S CLASSES_______________________________

# Choice of the model:
#=> the random Forest model performed sifnificantly better on the test set
#We choose the random Forest model for the prediciton of the testing data's classes

prediction.testing.data<-predict(modFit.rf,newdata=testing.data)
prediction.testing.data




###################################################################################
#                              ACKNOWLEDGEMENTS :                                 #
###################################################################################

# Data from: http://groupware.les.inf.puc-rio.br/har
# Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.
# Read more: http://groupware.les.inf.puc-rio.br/har#wle_paper_section#ixzz6NUg9fgbl