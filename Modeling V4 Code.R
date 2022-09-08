setwd("~/Desktop/Accenture/Modeling - Omit NAs/Model V4")
mydata <- read.csv("modelData_FL_IL_CO_V2_ALL.csv")
#---------------------------------------------------------
library(caret)
library(pROC)
library(DMwR)
options(scipen = 999)
#---------------------------------------------------------
mydata <- na.omit(mydata) #if we wanna omit NAs for modeling
#---------------------------------------------------------
#prepping dataset for model

#Changing Binary to Yes / No bc of Model Error
mydata$LILATracts_1And10 <- ifelse(mydata$LILATracts_1And10 == 1, "Yes", "No")
mydata$LILATracts_1And10 <- as.factor(mydata$LILATracts_1And10) #chaning response to factor
mydata$LILATracts_1And10 <- factor(mydata$LILATracts_1And10, levels = 
                                         c("Yes", "No")) #changing order of levels in df
mydata$State <- as.factor(mydata$State)

levels(mydata$LILATracts_1And10)# tests for correct order of levels
names <- as.data.frame(colnames(mydata))
bench.data <- mydata[, c(1,2,10,15,18,156,170,167,159,158,155,160,171,161,168,154,172)]
#---------------------------------------------------------
#Centering and Scaling DataSet
ppIndex <- bench.data[, -c(1:5)]
preProcValues <- preProcess(ppIndex, method = c('center', 'scale'))
mydataTransformed <- predict(preProcValues, bench.data)
bench.data <- mydataTransformed
#mydataTransformed <- mydataTransformed[, -2]
#bench.data <- cbind(LILATracts_1And10 = bench.data$LILATracts_1And10, mydataTransformed)
#---------------------------------------------------------
#partitioning dataframe 

set.seed(365) #random seed generator
partition.index <- createDataPartition(bench.data$LILATracts_1And10, p=0.7, list = FALSE) #partition index
train <- bench.data[partition.index, ] #train set
test <- bench.data[-partition.index, ] #test set

table(train$LILATracts_1And10) #check for proper ratios
table(test$LILATracts_1And10) #check for proper ratios
#---------------------------------------------------------
#trn_ctrl preprocess for modeling
trn_ctrl <- trainControl(summaryFunction = twoClassSummary, 
                         savePredictions = TRUE,
                         sampling = 'smote', 
                         method = 'repeatedcv', 
                         number = 5, 
                         repeats = 5, 
                         classProbs = TRUE, 
                         allowParallel = FALSE)
#---------------------------------------------------------
#MODELING
#MODELING
#MODELING
#---------------------------------------------------------
#Logit Model
logit_model <- train(LILATracts_1And10 ~ Property.Value..Standardized. + 
                       SNAPShare + OMultirShare + HousingUnits + 
                       TotalPopulation + Interior.Square.Feet + 
                       KidsShare + WalmartShare + SeniorsShare + 
                       HispanicShare + Lot.Size + 
                       DollarStoreShare + State, data = train, 
                     method = 'glm',
                     trControl = trn_ctrl,
                     metric = 'ROC', 
                     family = 'binomial')
#---------------------------------------------------------
#confusionMatrix for model predictions 
lvs <- c('Yes', 'No')
truth <- factor(logit_model$pred$obs)
pred <- factor(logit_model$pred$pred)
xtab <- table(pred, truth)
confusionMatrix(xtab)
#---------------------------------------------------------
#apply trained model to test data
pred_logit <- predict.train(object = logit_model, newdata = test, type = 'raw')
prob_logit <- predict.train(object = logit_model, newdata = test, type = 'prob')

#evaluating model performance on test data
confusionMatrix(pred_logit, test$LILATracts_1And10)
#---------------------------------------------------------
#saving model prob and raw results 
logit_output <- test
logit_output$pred <- pred_logit
logit_output$prob <- prob_logit
#---------------------------------------------------------
#create dataset for false positives 
false_positives <- logit_output %>%
  filter(LILATracts_1And10 == 'No' & pred == 'Yes')

false_negatives <- logit_output %>%
  filter(LILATracts_1And10 == 'Yes' & pred == 'No')

true_positives <- logit_output %>%
  filter(LILATracts_1And10 == 'Yes' & pred == 'Yes')
#---------------------------------------------------------
#creating variable importance plot
logit_importance <- varImp(logit_model, scale = FALSE)
plot(logit_importance)
#---------------------------------------------------------
#Saving Probabilities 
results <-NULL
results <- test
results$Logit_out <- prob_logit$Yes
SNAPShare_mean <- mean(results$SNAPShare)
Property.Value.Standardized_mean <- mean(results$Property.Value..Standardized.)
OmultirShare_mean <- mean(results$OMultirShare)
KidsShare_mean <- mean(results$KidsShare)
Interior.Square.Feet_mean <- mean(results$Interior.Square.Feet)
HousingUnits_mean <- mean(results$HousingUnits)
SeniorsShare_mean <- mean(results$SeniorsShare)
TotalPopulation_mean <- mean(results$TotalPopulation)
HispanicShare_mean <- mean(results$HispanicShare)
WalmartShare_mean <- mean(results$WalmartShare)
Lot.Size_mean <- mean(results$Lot.Size)
DollarStoreShare_mean <- mean(results$DollarStoreShare)
State_mean <- 'Florida'


iteration <- (6.9+1.2)/1683
test_bal_seq <- c(seq(from = -1.2, to = 6.9, by = iteration))


test_frame <- NULL
test_frame$LILATracts_1And10 <- results$LILATracts_1And10
test_frame <- as.data.frame(test_frame)

test_frame$SNAPShare <- SNAPShare_mean
test_frame$Property.Value..Standardized. <- Property.Value.Standardized_mean
test_frame$OMultirShare <- OmultirShare_mean
test_frame$KidsShare <- KidsShare_mean
test_frame$Interior.Square.Feet <- Interior.Square.Feet_mean
test_frame$HousingUnits <- HousingUnits_mean
test_frame$SeniorsShare <- SeniorsShare_mean
test_frame$TotalPopulation <- TotalPopulation_mean
test_frame$HispanicShare <- HispanicShare_mean
test_frame$WalmartShare <- WalmartShare_mean
test_frame$Lot.Size <- Lot.Size_mean
test_frame$DollarStoreShare <- DollarStoreShare_mean
test_frame$State <- State_mean
test_frame$SNAPShare <- test_bal_seq
#---------------------------------------------------------
test_frame_out <- predict(logit_model, newdata = test_frame, type = 'prob') 
test_frame$prob <- test_frame_out$Yes
ggplot(test_frame, aes(SNAPShare, prob)) + 
  geom_point()+
  geom_line()

