#Parameters Tuning for Decision Tree Model

InstaII.packages( "caret" )
install.packages(“rio“)
insta11. packages( "m1r" )
insta11. packages( "Metrics" )
install.packages(“rpart“)
install.packages(“rpart.plot”)
install.packages(“rpart.plot”)
install.packages(“imputeTS”)
library (tidyverse) library(imputeTS) 
library(ggplot) 
library(ggmap) 
library(kernlab)
library(caret) 
library(rio)
library(rpart)
library(rpart.plot)

data ‹- data.frame(read_csv('HM0_data.csv'))

data ‹- transform(
data, expensive= ifelse(cost > 5000, TRUE,FALSE))
data ‹ - data 1›1 mutate_at( . vars = c( "bm1" ) , 
                            .funs = -na_interpo1ation(.))

data <- data %›% mutate(across(bmi, -replace_na(., meau(., na.rm=TRUE))))
HNO_data <- data[, c('bmi', 'age','smoker', 'exercise', 'expensive')]
HNO_data <- data[, c('bmi', 'age','smoker', 'exercise', 'expensive')]
HNO_data$expensive ‹- as.factor(HM0_data$expensive)

set.seed(111)
trainList <- createDataPartition(y=HM0_data$expensive, p=.70,list=FALSE) 
trainset <- HM0_data[tnainList,]
testset <- HMO_data[-trainList,]

traiuSet$expensive ‹- as.factor(trainSet$expensive) 
trainSet$smoker ‹- as.factor(trainSet$smoker) 
trainSet$exercise ‹- as.factor(trainSet$exercise)

cartTree <- rpart(expensive-., data = trainSet,control = c(maxdepth = 5, cp=0.002))

prp(cantTnee, faclen = 0, cex = 0.8, extna = 1)

predictValues ‹- predict(cartTree, newdata=testSet, type = "class")
confusionMatrix(predictValues,as.factor(testSet$expensive) )

#confusion matrix and statistics 
library(mlr)	
library(Metrics)

d.tree.params <- makeClassifTask(
  data = trainset,
  target = "expensive"
  )

param_grid <- makeParamset(
  makeDiscreteParam("maxdepth", values = 1:30))

#Define Grid
control_grid = makeTuneControlGrid()
#Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
#Define Measure
measure = acc

set.seed(123)
dt_tuneparam <- tuneParams(learner="classif.rpart“, 
task=d.tree.params,
resampling = resample, 
measures = measure, 
par.set=param_grid, 
coutrol=control_grid, 
show.info = TRUE)

param_grid_multi <- makeParamSet(
makeDiscreteParam("maxdepth", value = 1:30),
makeNumericParam("cp", lower = 0.001, upper = 0.01),
makeDiscreteParam("minsplit", values = 1:30)
)

dt_tuneparam_multi ‹- tuneParams(learner=“classif.rpart“,
task = d.tree.params,
resampling = resample, 
measures = measune, 
par.set=param_grid_multi, 
control=control_grid, 
show.info = TRUE)

best_parameters_multi = setHyperPars( 
makeLearner(”classif.rpart”, predict.type = "prob"), 
par.vals = dt_tuneparam_multi$x
)
best_parameters_multi

cartTree <- rpart(expensive-., data = trainSet,coutrol = c(xval=0,maxdepth=14,cp=0.005,minsplit=5))

prp(cartTree, faclen = 0, cex = 0.8, extra = 1)

df ‹- data.frame(read_csv('HMO_TEST_data_sample.csv'))
df_sol ‹- data.frame(read_csv('HM0_TEST_data_sample_solution.csv')) 
testdf ‹- df[, c(’bmi', 'age','smoken', 'exercise')]

predictValues <- predict(cartTree, newdata=testdf, type = "class")
confusionMatrix(predictValues,as.factor(df_sol$expensive) )

predictValues <- predict(cartTree, newdata=testset, type = “class“) 
confusionMatrix(predictValues,as.factor(testSet$expensive) )





