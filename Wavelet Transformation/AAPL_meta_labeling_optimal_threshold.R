rm(list = setdiff(ls(), lsf.str()))
library(dplyr)
library(stringr)
library(caTools)
library(caret)
library(pROC)

# meta labeling logistic regression #

#import the dwt features
dwt_features <- read.csv("/Users/luzhang/Documents/GitHub/WrappingUp_Exploring_Intraday_Momentum/Data/AAPL_allSet_dwt_standardize.csv", header = T)
#head(dwt_features)
#dim(dwt_features)
#2622 57
dwt_features$X<-NULL

dwt_allSet<-data.frame(dwt_features)
#head(dwt_allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(dwt_allSet,1,function(x){sum(is.na(x))>0})
dwt_allSet <- subset(dwt_allSet, !idx_NA)
dwt_allSet$Y<-as.factor(dwt_allSet$Y)

#rolling split training and test set
library(rsample)

roll_eem_sliding <- 
  rolling_origin(
    data       = dwt_allSet,
    initial    = 1742,
    assess     = 220,
    skip = 219,
    cumulative = TRUE
  )

# define a function to find the optimal threshold on each rolling

optimal_threshold <- function(split){
  
  dwt_trainSet <- analysis(split)
  #primary model
  base_model<-glm(Y~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                  family = binomial(link="logit"),data=dwt_trainSet)
  
  #par(pty = "s")
  #roc(dwt_trainSet$Y, base_model$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, col="#377eb8", lwd=4)
  
  ## data used to make the ROC graph in a variable...
  roc.info <- roc(dwt_trainSet$Y, base_model$fitted.values, legacy.axes=TRUE)
  #str(roc.info)
  
  ## and then extract just the information that we want from that variable.
  roc.df <- data.frame(
    tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
    fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
    thresholds=roc.info$thresholds)
  
  ##the thresholds between TPP 90% and 95%...
  thresholds<-roc.df[roc.df$tpp >=80 & roc.df$tpp <=95,3]
  m<-length(thresholds)
  #head(thresholds)
  F1<-rep(0,m)
  
  train_Y<-dwt_trainSet$Y
  
  for (i in 1: m){
    
    train_probabs<-predict(base_model,dwt_trainSet,type="response")
    train_preds<-ifelse(train_probabs>1-thresholds[i],1,0) 
    dwt_trainSet$preds<-train_preds #adding predict labels as a new feature
    train_meta<-ifelse(train_Y==train_preds,1,0) #define metal labels: if Y==preds 1, else 0
    
    # Creating Secondary Model
    
    dwt_trainSet$Y<-NULL
    dwt_trainSet$meta_label<-train_meta
    
    meta_model<-glm(meta_label~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                    family = binomial(link="logit"),data=dwt_trainSet)
    train_probabs<-predict(meta_model,dwt_trainSet,type="response")
    meta_preds<-ifelse(train_probabs>0.5,1,0)
    
    # adjust predict label by meta value
    adj_label<-ifelse(meta_preds==1,train_preds,1-train_preds)
    cm_meta<-confusionMatrix(factor(adj_label), train_Y)
    F1[i]<-cm_meta$byClass[7]
  }
  return(thresholds[which.max(F1)])
}


rollingset01 <- roll_eem_sliding$splits[[1]]
rollingset02 <- roll_eem_sliding$splits[[2]]
rollingset03 <- roll_eem_sliding$splits[[3]]
rollingset04 <- roll_eem_sliding$splits[[4]]

opt_threshold01 <-optimal_threshold(rollingset01)
opt_threshold02 <-optimal_threshold(rollingset02)
opt_threshold03 <-optimal_threshold(rollingset03)
opt_threshold04 <-optimal_threshold(rollingset04)


