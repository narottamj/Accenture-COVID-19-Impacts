setwd("~/Desktop/Accenture/Modeling - Omit NAs")
mydata <- read.csv("OmitNA.model.df.csv")

library(caret)
library(pROC)

#modeling for LILA
#---------------------------------------------------------
#prepping dataset for model
bench.data <- mydata
bench.data$LILATracts_1And10 <- ifelse(bench.data$LILATracts_1And10 == 1, "Yes", "No")#Changing Binary to Yes / No bc of Model Error
bench.data$LILATracts_1And10 <- as.factor(bench.data$LILATracts_1And10) #chaning response to factor
bench.data$LILATracts_1And10 <- factor(bench.data$LILATracts_1And10, levels = 
                        c("Yes", "No")) #changing order of levels in df
levels(bench.data$LILATracts_1And10)# tests for correct order of levels

bench.data <- bench.data[, c(4, 17, 19, 21, 23:30, 42:54)]

#---------------------------------------------------------
#partitioning benchmark dataframe 

set.seed(365) #random seed generator
partition.index <- createDataPartition(bench.data$LILATracts_1And10, p=0.6, list = FALSE) #partition index
bench_train <- bench.data[partition.index, ] #train set
bench_test <- bench.data[-partition.index, ] #test set

table(bench_train$LILATracts_1And10) # 11.7%
table(bench_test$LILATracts_1And10)

#---------------------------------------------------------
#benchmark Logit Model

trn_ctrl <- trainControl(summaryFunction = twoClassSummary, 
                         savePredictions = TRUE, 
                         method = 'repeatedcv', 
                         number = 5, 
                         repeats = 5, 
                         classProbs = TRUE, 
                         allowParallel = FALSE) # I think this is K-fold CV

logit_bench <- train(LILATracts_1And10~., data = bench_train, 
                   method = 'glm',
                   trControl = trn_ctrl, 
                   family = 'binomial' ) # created Logit model

#---------------------------------------------------------
#confusionMatrix for benchmark model predictions 

lvs <- c('Yes', 'No')
truth <- factor(logit_bench$pred$obs)
pred <- factor(logit_bench$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab) # Accuracy is 87.3% 

#---------------------------------------------------------
#apply trained benchmark model to test data

pred_logit_bench <- predict.train(object = logit_bench, newdata = bench_test, type = 'raw')
prob_logit_bench <- predict.train(object = logit_bench, newdata = bench_test, type = 'prob')

#evaluating model performance on test data
confusionMatrix(pred_logit_bench, bench_test$LILATracts_1And10) # Accuracy is 87.58%

#saving logit model test data results
logit_bench_output <- bench_test
logit_bench_output$pred <- pred_logit_bench
logit_bench_output$prob <- prob_logit_bench

#now we can make a confusion matrix with this new dataset 
confusionMatrix(logit_bench_output$pred, logit_bench_output$LILATracts_1And10)

#---------------------------------------------------------





#---------------------------------------------------------
#PCA 
pca.data <- mydata[, c(17, 19, 21, 23:30, 42:54)] #predictor variables only
pca <- princomp(pca.data, cor = TRUE) #PCA output
summary(pca) #include up to Comp.8
loadings(pca) #if you want to be able to explain PCA Comps...not relevant as of now
biplot(pca) #plot showing us Comp 1 and 2 variables 

#---------------------------------------------------------
#creating PCA dataframe 
scores <- as.data.frame(pca$scores[, 1:8]) #df containing 1-8 Comps
pca.df <- cbind(LILA = mydata$LILATracts_1And10, scores) #merging response variable with PCA dataframe

pca.df$LILA <- ifelse(pca.df$LILA == 1, "Yes", "No")#Changing Binary to Yes / No bc of Model Error

pca.df$LILA <- as.factor(pca.df$LILA) #chaning response to factor

pca.df$LILA <- factor(pca.df$LILA, levels = 
                                   c("Yes", "No")) #changing order of levels in df

levels(pca.df$LILA)# tests for correct order of levels

#pca.df is the dataset we will use to model LI with PCA attributes 

#---------------------------------------------------------
#partitioning PCA dataframe

set.seed(365) #random seed generator
partition.index <- createDataPartition(pca.df$LILA, p=0.7, list = FALSE) #partition index
pca_train <- pca.df[partition.index, ] #train set
pca_test <- pca.df[-partition.index, ] #test set

table(pca_train$LILA) # 11.76%
table(pca_test$LILA) 

#---------------------------------------------------------
#PCA Logit Model

trn_ctrl <- trainControl(summaryFunction = twoClassSummary, 
                         savePredictions = TRUE, 
                         method = 'repeatedcv', 
                         number = 5, 
                         repeats = 5, 
                         classProbs = TRUE, 
                         allowParallel = FALSE) # I think this is K-fold CV

logit_pca <- train(LILA~., data = pca_train, 
                   method = 'glm',
                   trControl = trn_ctrl, 
                   family = 'binomial' ) # created Logit model

#---------------------------------------------------------
#confusionMatrix for model predictions 

lvs <- c('Yes', 'No')
truth <- factor(logit_pca$pred$obs)
pred <- factor(logit_pca$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab) # Accuracy is 87.59% for train data...lets move onto test

#---------------------------------------------------------
#apply trained model to test data

pred_logit_pca <- predict.train(object = logit_pca, newdata = pca_test, type = 'raw')
prob_logit_pca <- predict.train(object = logit_pca, newdata = pca_test, type = 'prob')

#evaluating model performance on test data
confusionMatrix(pred_logit_pca, pca_test$LILA) # Accuracy is 88.41%

#saving logit model test data results
logit_pca_output <- pca_test
logit_pca_output$pred <- pred_logit_pca
logit_pca_output$prob <- prob_logit_pca

#now we can make a confusion matrix with this new dataset 
confusionMatrix(logit_pca_output$pred, logit_pca_output$LILA)

#---------------------------------------------------------
#creating ROC plot

pred_tst <- predict(logit_pca, newdata = pca_test)
pca_test$pred <- pred_tst
pca_test$prob <- prob_logit_pca$Yes

myROC <- roc(response = pca_test$LILA, predictor = pca_test$prob, levels = 
               c("Yes", "No"))

myROC # to get AUC
plot(myROC)

#---------------------------------------------------------





#---------------------------------------------------------
# PCA Random Forest 

set.seed(365)

trn_ctrl <- trainControl(summaryFunction = twoClassSummary, 
                         savePredictions = TRUE, 
                         method = 'repeatedcv', 
                         number = 5, 
                         repeats = 5, 
                         classProbs = TRUE, 
                         allowParallel = FALSE) # I think this is K-fold CV 

mtry <- 1:3
tunegrid <- expand.grid(.mtry=mtry)
rf_pca <- train(LILA~., data = pca_train, 
                method = 'rf', 
                metric = 'ROC', 
                tuneGrid = tunegrid, 
                trControl = trn_ctrl)

#RF with tuneLength controlling hyperparameters
rf_pca <- train(LILA~., data = pca_train, 
                method = 'rf', 
                metric = 'ROC', 
                tuneLength = 4, 
                trControl = trn_ctrl)

#---------------------------------------------------------
#confusionMatrix for model predictions 

lvs <- c('Yes', 'No')
truth <- factor(rf_pca$pred$obs)
pred <- factor(rf_pca$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab) # 88.59% accuracy with .6p

#---------------------------------------------------------
#apply trained model to test data

pred_rf_pca <- predict.train(object = rf_pca, newdata = pca_test, type = 'raw')
prob_rf_pca <- predict.train(object = rf_pca, newdata = pca_test, type = 'prob')

#evaluating model performance on test data
confusionMatrix(pred_rf_pca, pca_test$LILA) # Accuracy is 88.16% with .6p

#saving model test data results
logit_pca_output <- pca_test
logit_pca_output$pred <- pred_logit_pca
logit_pca_output$prob <- prob_logit_pca

#now we can make a confusion matrix with this new dataset 
confusionMatrix(logit_pca_output$pred, logit_pca_output$LILA)

#---------------------------------------------------------
#creating ROC plot

pred_tst <- predict(rf_pca, newdata = pca_test)
pca_test$pred <- pred_tst
pca_test$prob <- prob_rf_pca$Yes

myROC <- roc(response = pca_test$LILA, predictor = pca_test$prob, levels = 
               c("Yes", "No"))

myROC # to get AUC
plot(myROC)

#---------------------------------------------------------





#---------------------------------------------------------
#PCA svmLinear 
modelLookup('svmLinear')

trn_ctrl <- trainControl(summaryFunction = twoClassSummary, 
                         savePredictions = TRUE, 
                         method = 'repeatedcv', 
                         number = 5, 
                         repeats = 5, 
                         classProbs = TRUE, 
                         allowParallel = FALSE) # I think this is K-fold CV

svm_pca <- train(LILA~., data = pca_train, 
                 method = 'svmLinear',
                 tuneLength = 4, 
                 trControl = trn_ctrl)

#---------------------------------------------------------
#confusionMatrix for model predictions 

lvs <- c('Yes', 'No')
truth <- factor(svm_pca$pred$obs)
pred <- factor(svm_pca$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab) # 88.17% accuracy 

#---------------------------------------------------------





#---------------------------------------------------------
#PCA xgb
modelLookup('xgbLinear')

trn_ctrl <- trainControl(summaryFunction = twoClassSummary, 
                         savePredictions = TRUE, 
                         method = 'repeatedcv', 
                         number = 5, 
                         repeats = 5, 
                         classProbs = TRUE, 
                         allowParallel = FALSE) # I think this is K-fold CV

xgb_pca <- train(LILA~., data = pca_train, 
                 method = 'xgbLinear', 
                 metric = 'ROC', 
                 trControl = trn_ctrl)

#---------------------------------------------------------
#confusionMatrix for model predictions 

lvs <- c('Yes', 'No')
truth <- factor(xgb_pca$pred$obs)
pred <- factor(xgb_pca$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab) # 87.1% accuracy 

#---------------------------------------------------------
#apply trained model to test data

pred_xgb_pca <- predict.train(object = xgb_pca, newdata = pca_test, type = 'raw')
prob_xgb_pca <- predict.train(object = xgb_pca, newdata = pca_test, type = 'prob')

#evaluating model performance on test data
confusionMatrix(pred_xgb_pca, pca_test$LILA) # Accuracy is 86.09%

#saving model test data results
xgb_pca_output <- pca_test
xgb_pca_output$pred <- pred_xgb_pca
xgb_pca_output$prob <- prob_xgb_pca

#now we can make a confusion matrix with this new dataset 
confusionMatrix(xgb_pca_output$pred, xgb_pca_output$LILA)

#---------------------------------------------------------
#creating ROC plot

pred_tst <- predict(xgb_pca, newdata = pca_test)
pca_test$pred <- pred_tst
pca_test$prob <- prob_xgb_pca$Yes

myROC <- roc(response = pca_test$LILA, predictor = pca_test$prob, levels = 
               c("Yes", "No"))

myROC # to get AUC
plot(myROC)

