
################# Logistic Regression #################

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

#logist regression
model_lr<-glm(Y~., family = binomial(link="logit"),data=trainSet)

summary(model_lr)# 4 not defined because of singularities probably owing to there exists multicollinearity in here

# remove covariates raw_BBandsup raw_DonchianChannelL, raw_SMAClose, raw_runSum
model_lr<-glm(Y~.-raw_BBandsup-raw_DonchianChannelL-raw_SMAClose-raw_runSum , family = binomial(link="logit"),data=trainSet)

probabs<-predict(model_lr,testSet,type="response")
preds<-ifelse(probabs>0.5,1,0)
cm_lr<-confusionMatrix(factor(preds), factor(testSet$Y))

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 359  74
#1  83 358

#Accuracy : 0.8204          
#95% CI : (0.7933, 0.8453)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.6408          

#Mcnemar's Test P-Value : 0.5232          
                                          
#            Sensitivity : 0.8122          
#            Specificity : 0.8287          
#         Pos Pred Value : 0.8291          
#         Neg Pred Value : 0.8118          
#             Prevalence : 0.5057          
#         Detection Rate : 0.4108          
#   Detection Prevalence : 0.4954          
#      Balanced Accuracy : 0.8205          
                                          
#       'Positive' Class : 0 


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
  model_lr<-glm(Y~.-raw_BBandsup-raw_DonchianChannelL-raw_SMAClose-raw_runSum , 
                family = binomial(link="logit"),
                data=analysis_set)
  
  testSet = assessment(split)
  probabs<-predict(model_lr,testSet,type="response")
  preds<-ifelse(probabs>0.5,1,0)
  cm_lr<-confusionMatrix(factor(preds), factor(testSet$Y))
  return(cm_lr)
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


