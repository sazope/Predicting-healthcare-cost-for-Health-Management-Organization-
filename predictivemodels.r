#Library to call at the begining of the code
library (tidyverse)
library(RCurl)
library(jsonlite)
library(imputeTS)
library(ggmap)
library(kernlab)
library(caret)
library(rio)
library(rpart) 
library(rpart.plot)

data<- data.frame(read_csv('HMO_data.csv'))
data<- transform(data, expensive= ifelse(cost > 5000, TRUE, FALSE))
data<- data%>% mutate(across(bmi, ~replace_na(., mean(., na.rm=TRUE))))

HMO_data <- data[, c( 'bmi', 'age','smoker', 'exercise', 'cost')]

output<- lm(cost~., data= HMO_data)
summary(output)

HMO data <- data[, c( 'bmi', 'age',' smoker', 'exercise', 'expensive')]
HMO_data$expensive <- as.factor(HMO_data$expensive)

set.seed(lll)
trainList <- createDataPartition(y=HMO_data$expensive, p=.70,list=FALSE) trainSet <- HMO_data[trainList,]
testSet <- HMO_data[-trainList,]

model<- ksvm(data= trainSet, expensive~., C=5, CV =3, prob.model =TRUE)
model

svmPred <- predict(model,newdata = testSet)

confMatrix <- table(svmPred, testSet$expensive)
confMatrix[l, "FALSE"]
confMatrix

confusionMatrix(svmPred,testSet$expensive)

cartTree <- rpart(expensive~., data= trainSet, control= c(maxdepth = 5, cp=0.002))
prp(cartTree, faclen = 0, cex = 0.8, extra= 1)

predictValues <- predict(cartTree, newdata=testSet, type= ''class")
confusionMatrix(predictValues,testSet$expensive)








