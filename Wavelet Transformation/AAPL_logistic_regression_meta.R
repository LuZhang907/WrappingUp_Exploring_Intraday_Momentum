######################  standardized dwt dataset      ###################

rm(list = setdiff(ls(), lsf.str()))
library(dplyr)
library(stringr)
library(caTools)
library(caret)
library(pROC)
library(rsample)

# meta labeling logistic regression #

#import the dwt features
features <- read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum_2rd_try/Data/AAPL_allSet_dwt_standardize.csv", header = T)
#head(dwt_features)
#dim(dwt_features)
#2622 57
features$X<-NULL

allSet<-data.frame(features)
#head(raw_allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
allSet <- subset(allSet, !idx_NA)
allSet$Y<-as.factor(allSet$Y)

# define a function which could give a evalutaion matrix for each rolling
rolling_evaluation_matrix <- function(split,threshold){
  trainSet <- analysis(rollingset01) 
  testSet <- assessment(rollingset01)
  #primary model
  
  train_Y<-trainSet$Y
  
  base_model<-glm(Y~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                  family = binomial(link="logit"),data=trainSet)
  train_probabs<-predict(base_model,trainSet,type="response")
  train_preds<-ifelse(train_probabs>1-threshold,1,0) # threshold achieve highest F1 scores on trainset
  trainSet$preds<-train_preds #adding predict labels as a new feature
  train_meta<-ifelse(train_Y==train_preds,1,0) #define metal labels: if Y==preds 1, else 0
  
  # Creating Secondary Model
  
  trainSet$Y<-NULL
  trainSet$meta_label<-train_meta
  
  meta_model<-glm(meta_label~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                  family = binomial(link="logit"),data=trainSet)
  
  # introducing test set
  test_Y<-testSet$Y
  
  # test set on primary model, produce predict y labels
  test_probabs<-predict(base_model,testSet,type="response")
  test_preds<-ifelse(test_probabs>1-threshold,1,0) # threshold achieve highest F1 scores on trainset
  testSet$preds<-test_preds #adding predict labels as a new feature
  test_meta<-ifelse(test_Y==test_preds,1,0) 
  
  # test set on secondary model
  testSet$Y<-NULL
  testSet$meta_label<-test_meta
  
  test_probabs<-predict(meta_model,testSet,type="response")
  meta_preds<-ifelse(test_probabs>0.5,1,0)
  
  # adjust predict label by meta value
  adj_label<-ifelse(meta_preds==1,test_preds,1-test_preds)
  cm_meta<-confusionMatrix(factor(adj_label), factor(test_Y))
  return(cm_meta)
}


# rolling

roll_eem_sliding <- 
  rolling_origin(
    data       = allSet,
    initial    = 1742,
    assess     = 220,
    skip = 219,
    cumulative = TRUE
  )

rollingset01 <- roll_eem_sliding$splits[[1]]
rollingset02 <- roll_eem_sliding$splits[[2]]
rollingset03 <- roll_eem_sliding$splits[[3]]
rollingset04 <- roll_eem_sliding$splits[[4]]

thresholds<-c(0.2867242,0.4939357,0.4930356,0.4601947)

rolling01 <-rolling_evaluation_matrix(rollingset01,thresholds[1])
rolling02 <-rolling_evaluation_matrix(rollingset02,thresholds[2])
rolling03 <-rolling_evaluation_matrix(rollingset03,thresholds[3])
rolling04 <-rolling_evaluation_matrix(rollingset04,thresholds[4])

