rm(list = setdiff(ls(), lsf.str()))
library(dplyr)
library(stringr)
library(caTools)
library(caret)
library(pROC)

######################  standardized raw dataset      ###################
########################################################################

# meta labeling logistic regression #

#import the raw features
raw_features <- read.csv("/Users/luzhang/Desktop/AAPL_features/allSet_raw_standardize.csv", header = T)
#head(raw_features)
#dim(raw_features)
#2662 57
raw_features$X<-NULL

raw_allSet<-data.frame(raw_features)
#head(raw_allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(raw_allSet,1,function(x){sum(is.na(x))>0})
raw_allSet <- subset(raw_allSet, !idx_NA)
raw_allSet$Y<-as.factor(raw_allSet$Y)

#table(raw_allSet$Y)
#0 1
#1318 1304

nx <- nrow(raw_allSet)
raw_trainSet <- raw_allSet[1:floor(nx*2/3),]
raw_testSet <- raw_allSet[(floor(nx*2/3)+1):nx,]

dim(raw_allSet);
# 2662 56 
dim(raw_trainSet); 
#1748   56
dim(raw_testSet)
# 874  56

table(raw_trainSet$Y)
#0   1 
#876 872 

table(raw_testSet$Y)
#0   1 
#442 432 

#primary model

train_Y<-raw_trainSet$Y

base_model<-glm(Y~.-raw_BBandsup-raw_DonchianChannelL-raw_SMAClose-raw_runSum,
                family = binomial(link="logit"),data=raw_trainSet)
train_probabs<-predict(base_model,raw_trainSet,type="response")
train_preds<-ifelse(train_probabs>1-0.3468271,1,0) # threshold achieve highest F1 scores on trainset
raw_trainSet$preds<-train_preds #adding predict labels as a new feature
train_meta<-ifelse(train_Y==train_preds,1,0) #define metal labels: if Y==preds 1, else 0

# Creating Secondary Model

raw_trainSet$Y<-NULL
raw_trainSet$meta_label<-train_meta

meta_model<-glm(meta_label~.-raw_BBandsup-raw_DonchianChannelL-raw_SMAClose-raw_runSum,
                family = binomial(link="logit"),data=raw_trainSet)

# introducing test set
test_Y<-raw_testSet$Y

# test set on primary model, produce predict y labels
test_probabs<-predict(base_model,raw_testSet,type="response")
test_preds<-ifelse(test_probabs>1-0.3468271,1,0) # threshold achieve highest F1 scores on trainset
raw_testSet$preds<-test_preds #adding predict labels as a new feature
test_meta<-ifelse(test_Y==test_preds,1,0) 

# test set on secondary model
raw_testSet$Y<-NULL
raw_testSet$meta_label<-test_meta

test_probabs<-predict(meta_model,raw_testSet,type="response")
meta_preds<-ifelse(test_probabs>0.5,1,0)

# adjust predict label by meta value
adj_label<-ifelse(meta_preds==1,test_preds,1-test_preds)
cm_meta<-confusionMatrix(factor(adj_label), test_Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 371 104
#1  71 328

#Accuracy : 0.7998          
#95% CI : (0.7717, 0.8258)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.5991          

#Mcnemar's Test P-Value : 0.01556         

#            Sensitivity : 0.8394          
#            Specificity : 0.7593          
#        Pos Pred Value : 0.7811          
#         Neg Pred Value : 0.8221          
#            Prevalence : 0.5057          
#        Detection Rate : 0.4245          
#  Detection Prevalence : 0.5435          
#     Balanced Accuracy : 0.7993          

#       'Positive' Class : 0  

# original logistic regression #

raw_trainSet <- raw_allSet[1:floor(nx*2/3),]
raw_testSet <- raw_allSet[(floor(nx*2/3)+1):nx,]

base_model<-glm(Y~.-raw_BBandsup-raw_DonchianChannelL-raw_SMAClose-raw_runSum,
                family = binomial(link="logit"),data=raw_trainSet)

probabs<-predict(base_model,raw_testSet,type="response")
preds<-ifelse(probabs>0.5,1,0)
cm_base<-confusionMatrix(factor(preds), factor(raw_testSet$Y))

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 361  80
#1  81 352

#Accuracy : 0.8158         
#95% CI : (0.7885, 0.841)
#No Information Rate : 0.5057         
#P-Value [Acc > NIR] : <2e-16         

#Kappa : 0.6315         

#Mcnemar's Test P-Value : 1              

#            Sensitivity : 0.8167         
#            Specificity : 0.8148         
#         Pos Pred Value : 0.8186         
#         Neg Pred Value : 0.8129         
#             Prevalence : 0.5057         
#         Detection Rate : 0.4130         
#   Detection Prevalence : 0.5046         
#      Balanced Accuracy : 0.8158         

#       'Positive' Class : 0    

models <- list(base=base_model, meta=meta_model)
library(dplyr)
comparison <- data.frame(model = names(models),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))


for (name in names(models)) {
  model <- get(paste0("cm_", name))
  class<-model$byClass
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(
      Specificity = class["Specificity"],
      Precision = class["Precision"],
      Recall = class["Recall"],
      F1 = class["F1"])
}
comparison

#model Specificity Precision    Recall        F1
#1  base   0.8148148 0.8185941 0.8167421 0.8176670
#2  meta   0.7592593 0.7810526 0.8393665 0.8091603

write.csv(comparison,"/Users/luzhang/Desktop/AAPL_features/raw_standardized_meta.csv")


######################  standardized dwt dataset      ###################
########################################################################
rm(list = setdiff(ls(), lsf.str()))
library(dplyr)
library(stringr)
library(caTools)
library(caret)
library(pROC)
# meta labeling logistic regression #

#import the dwt features
dwt_features <- read.csv("/Users/luzhang/Desktop/AAPL_features/allSet_dwt_standardize.csv", header = T)
#head(dwt_features)
#dim(dwt_features)
#2622 57
dwt_features$X<-NULL

dwt_allSet<-data.frame(dwt_features)
#head(raw_allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(dwt_allSet,1,function(x){sum(is.na(x))>0})
dwt_allSet <- subset(dwt_allSet, !idx_NA)
dwt_allSet$Y<-as.factor(dwt_allSet$Y)
#dim(raw_allSet)
# 2662 56 
#table(raw_allSet$Y)
#0 1
#1318 1304
nx <- nrow(dwt_allSet)
dwt_trainSet <- dwt_allSet[1:floor(nx*2/3),]
dwt_testSet <- dwt_allSet[(floor(nx*2/3)+1):nx,]
dim(dwt_allSet); dim(dwt_trainSet); dim(dwt_testSet)
#[1] 2622   56
#[1] 1748   56
#[1] 874  56

#primary model

train_Y<-dwt_trainSet$Y

base_model<-glm(Y~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                family = binomial(link="logit"),data=dwt_trainSet)
train_probabs<-predict(base_model,dwt_trainSet,type="response")
train_preds<-ifelse(train_probabs>1-0.4499688,1,0) # threshold achieve highest F1 scores on trainset
dwt_trainSet$preds<-train_preds #adding predict labels as a new feature
train_meta<-ifelse(train_Y==train_preds,1,0) #define metal labels: if Y==preds 1, else 0

# Creating Secondary Model

dwt_trainSet$Y<-NULL
dwt_trainSet$meta_label<-train_meta

meta_model<-glm(meta_label~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                family = binomial(link="logit"),data=dwt_trainSet)

# introducing test set
test_Y<-dwt_testSet$Y

# test set on primary model, produce predict y labels
test_probabs<-predict(base_model,dwt_testSet,type="response")
test_preds<-ifelse(test_probabs>1-0.4499688,1,0) # threshold achieve highest F1 scores on trainset
dwt_testSet$preds<-test_preds #adding predict labels as a new feature
test_meta<-ifelse(test_Y==test_preds,1,0) 

# test set on secondary model
dwt_testSet$Y<-NULL
dwt_testSet$meta_label<-test_meta

test_probabs<-predict(meta_model,dwt_testSet,type="response")
meta_preds<-ifelse(test_probabs>0.5,1,0)

# adjust predict label by meta value
adj_label<-ifelse(meta_preds==1,test_preds,1-test_preds)
cm_meta<-confusionMatrix(factor(adj_label), test_Y)

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 378  89
#1  64 343

#Accuracy : 0.8249          
#95% CI : (0.7981, 0.8496)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.6496          

#Mcnemar's Test P-Value : 0.05235         

#            Sensitivity : 0.8552          
#            Specificity : 0.7940          
#         Pos Pred Value : 0.8094          
#        Neg Pred Value : 0.8428          
#            Prevalence : 0.5057          
#         Detection Rate : 0.4325          
#   Detection Prevalence : 0.5343          
#      Balanced Accuracy : 0.8246          

#      'Positive' Class : 0 

# original logistic regression #

dwt_trainSet <- dwt_allSet[1:floor(nx*2/3),]
dwt_testSet <- dwt_allSet[(floor(nx*2/3)+1):nx,]

base_model<-glm(Y~.-dwt_BBandsup-dwt_DonchianChannelL-dwt_SMAClose-dwt_runSum,
                family = binomial(link="logit"),data=dwt_trainSet)

probabs<-predict(base_model,dwt_testSet,type="response")
preds<-ifelse(probabs>0.5,1,0)
cm_base<-confusionMatrix(factor(preds), factor(dwt_testSet$Y))

#Confusion Matrix and Statistics

#Reference
#Prediction   0   1
#0 368  79
#1  74 353

#Accuracy : 0.8249          
#95% CI : (0.7981, 0.8496)
#No Information Rate : 0.5057          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.6498          

#Mcnemar's Test P-Value : 0.7464          

#            Sensitivity : 0.8326          
#           Specificity : 0.8171          
#         Pos Pred Value : 0.8233          
#         Neg Pred Value : 0.8267          
#            Prevalence : 0.5057          
#        Detection Rate : 0.4211          
#   Detection Prevalence : 0.5114          
#      Balanced Accuracy : 0.8249          

#       'Positive' Class : 0   

models <- list(base=base_model, meta=meta_model)
library(dplyr)
comparison <- data.frame(model = names(models),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))


for (name in names(models)) {
  model <- get(paste0("cm_", name))
  class<-model$byClass
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(
      Specificity = class["Specificity"],
      Precision = class["Precision"],
      Recall = class["Recall"],
      F1 = class["F1"])
}
comparison
#model Specificity Precision    Recall        F1
#1  base   0.8171296 0.8232662 0.8325792 0.8278965
#2  meta   0.7939815 0.8094218 0.8552036 0.8316832

write.csv(comparison,"/Users/luzhang/Desktop/AAPL_features/dwt_standardized_meta.csv")