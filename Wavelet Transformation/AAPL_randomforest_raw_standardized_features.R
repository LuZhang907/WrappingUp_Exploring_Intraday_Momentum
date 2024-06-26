
################# random forest #################

rm(list = setdiff(ls(), lsf.str()))
library(fmlr)
library(quantmod)
library(randomForestFML)
library(ROCR)
library(caret)

features <- read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_allSet_raw_standardize.csv", header = T)
head(features)
dim(features)
#2622 57
features$X<-NULL


allSet<-data.frame(features)
head(allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
allSet <- subset(allSet, !idx_NA)
allSet$Y<-as.factor(allSet$Y)
dim(allSet)
# 2622 56 
table(allSet$Y)
#0 1
#1318 1304
nx <- nrow(allSet)
trainSet <- allSet[1:floor(nx*2/3),]
testSet <- allSet[(floor(nx*2/3)+1):nx,]
dim(allSet); dim(trainSet); dim(testSet)
#[1] 2622  56
#[1] 1748   56
#[1] 874  56

table(trainSet$Y)
#0    1 
#876 872
table(testSet$Y)
#0   1 
#442 432

####original  random forest 

set.seed(1)
model_rf <- caret::train(Y ~ .,
                         data = trainSet,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
final <- data.frame(actual = testSet$Y,
                    predict(model_rf, newdata = testSet, type = "prob"))
final$predict <- ifelse(final$X0 > 0.5, 0, 1)
cm_original <- confusionMatrix(as.factor(final$predict), testSet$Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 348  77
#1  94 355

#Accuracy : 0.8043          
#95% CI : (0.7765, 0.8302)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.6088          

#Mcnemar's Test P-Value : 0.2211          

#            Sensitivity : 0.7873          
#            Specificity : 0.8218          
#         Pos Pred Value : 0.8188          
#         Neg Pred Value : 0.7906          
#             Prevalence : 0.5057          
#         Detection Rate : 0.3982          
#   Detection Prevalence : 0.4863          
#      Balanced Accuracy : 0.8045          

#       'Positive' Class : 0  

#################  rolling_origin ################
library(rsample)
library(tidyverse)
library(tidyquant)

roll_eem_sliding <- 
  rolling_origin(
    data       = allSet,
    initial    = 1742,
    assess     = 220,
    skip = 219,
    cumulative = TRUE
  )

library(yardstick)

rolling_evaluation_matrix <- function(split){
  set.seed(1)
  analysis_set <- analysis(split) 
  model_rf <- caret::train(Y ~ .,
                           data = analysis_set,
                           method = "rf",
                           preProcess = c("scale", "center")
                           )
  testSet = assessment(split)
  final <- data.frame(actual = testSet$Y,
                      predict(model_rf, newdata = testSet, type = "prob"))
  final$predict <- ifelse(final$X0 > 0.5, 0, 1)
  cm_original <- confusionMatrix(as.factor(final$predict), testSet$Y)
  return(cm_original)
}

rollingset01 <- roll_eem_sliding$splits[[1]]
rollingset02 <- roll_eem_sliding$splits[[2]]
rollingset03 <- roll_eem_sliding$splits[[3]]
rollingset04 <- roll_eem_sliding$splits[[4]]

table(assessment(rollingset01)$Y)


rolling01 <-rolling_evaluation_matrix(rollingset01)
rolling02 <-rolling_evaluation_matrix(rollingset02)
rolling03 <-rolling_evaluation_matrix(rollingset03)
rolling04 <-rolling_evaluation_matrix(rollingset04)


