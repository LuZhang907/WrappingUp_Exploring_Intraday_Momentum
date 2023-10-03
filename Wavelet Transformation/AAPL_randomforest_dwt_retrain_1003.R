################# random forest #################

rm(list = setdiff(ls(), lsf.str()))
#library(fmlr)
library(quantmod)
library(TTR) # for various indicators
library(randomForest)
library(ROCR)
library(caret)
library(varImp)

features <- read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum_2rd_try/Data/AAPL_allSet_dwt_standardize.csv", header = T)
head(features)
dim(features)
#2662 57
features$X<-NULL


allSet<-data.frame(features)
head(allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
allSet <- subset(allSet, !idx_NA)
allSet$Y<-as.factor(allSet$Y)
dim(allSet)
# 2662 56
table(allSet$Y)
#0 1
#1318 1304
nx <- nrow(allSet)
trainSet <- allSet[1:floor(nx*2/3),]
testSet <- allSet[(floor(nx*2/3)+1):nx,]
dim(allSet); dim(trainSet); dim(testSet)
#[1] 2622   56
#[1] 1748   56
#[1] 874  56

table(trainSet$Y)
#0   1 
#876 872 
table(testSet$Y)
#0   1 
#442 432 

#### original  random forest 
set.seed(1)
control <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
metric <- "F1 score"
mtry <- 25
model_rf <- train(Y ~ .,
                         data = trainSet,
                         method = "rf",
                         metric = metric,
                         trControl = control)


model_rf <- randomForest(Y~., data = trainSet, importance = TRUE)
importance = importance(model_rf)
varImpPlot(model_rf, n.var = 20, main = "")



final <- data.frame(actual = testSet$Y,
                    predict(model_rf, newdata = testSet, type = "prob"))
final$predict <- ifelse(final$X0 > 0.5, 0, 1)
cm_original <- confusionMatrix(as.factor(final$predict), testSet$Y)
cm_original


#################  rolling_origin ################
library(rsample)

roll_eem_sliding <- 
  rolling_origin(
    data       = allSet,
    initial    = 1742,
    assess     = 220,
    skip = 219,
    cumulative = TRUE
  )

#library(yardstick)

rolling_evaluation_matrix <- function(split){
  set.seed(1)
  analysis_set <- analysis(split) 
  model_rf <- randomForest(Y~., data = analysis_set, importance = TRUE)
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

