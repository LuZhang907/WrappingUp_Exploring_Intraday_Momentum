
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
  
  class <- cm_original$byClass
  Accuracy = cm_original$overall[1]
  Specificity = class["Specificity"]
  Precision = class["Precision"]
  Recall = class["Recall"]
  F1 = class["F1"]
  return(list(Accuracy,Specificity,Precision,Recall,F1))
}

test01 <-rolling_evaluation_matrix(roll_eem_sliding$splits[[1]])
test02 <-rolling_evaluation_matrix(roll_eem_sliding$splits[[2]])
test03 <-rolling_evaluation_matrix(roll_eem_sliding$splits[[3]])
test04 <-rolling_evaluation_matrix(roll_eem_sliding$splits[[4]])
################    under-sampling  #######################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")
set.seed(1)
model_rf_under <- caret::train( Y~ .,
                                data = trainSet,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
final_under <- data.frame(actual = testSet$Y,
                          predict(model_rf_under, newdata = testSet, type = "prob"))
final_under$predict <- ifelse(final_under$X0 > 0.5, 0, 1)
cm_under <- confusionMatrix(as.factor(final_under$predict), testSet$Y)


################## Over Sampling ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")
set.seed(1)
model_rf_over <- caret::train( Y~ .,
                               data = trainSet,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_over <- data.frame(actual = testSet$Y,
                         predict(model_rf_over, newdata = testSet, type = "prob"))
final_over$predict <- ifelse(final_over$X0 > 0.5, 0, 1)
cm_over <- confusionMatrix(as.factor(final_over$predict), testSet$Y)


################## Rose ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")
set.seed(1)
model_rf_rose <- caret::train( Y~ .,
                               data = trainSet,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_rose <- data.frame(actual = testSet$Y,
                         predict(model_rf_rose, newdata = testSet, type = "prob"))
final_rose$predict <- ifelse(final_rose$X0 > 0.5, 0, 1)
cm_rose <- confusionMatrix(as.factor(final_rose$predict), testSet$Y)



################## smote ##################

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")
set.seed(1)
model_rf_smote <- caret::train( Y~ .,
                                data = trainSet,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = ctrl)
final_smote <- data.frame(actual = testSet$Y,
                          predict(model_rf_smote, newdata = testSet, type = "prob"))
final_smote$predict <- ifelse(final_smote$X0 > 0.5, 0, 1)
cm_smote <- confusionMatrix(as.factor(final_smote$predict), testSet$Y)


########### compare predictions ########

models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)
resampling <- resamples(models)
bwplot(resampling)

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



write.csv(comparison, "/Users/luzhang/Desktop/AAPL_features/comparsion_raw_standardized.csv")

library(tidyr)
comparison %>%
  gather(x, y, Specificity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)


