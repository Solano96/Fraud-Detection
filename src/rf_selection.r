library(randomForest)
library(ggplot2)
library(funModeling)
library("ROSE")

data_train <- read.csv(file = "./data/data_train.csv", header = TRUE, sep  = ",")
data_test <- read.csv(file = "./data/data_test.csv", header = TRUE, sep  = ",")

as.data.frame(table(data_train$isFraud))

data_train <- ovun.sample(isFraud~.,data = data_train, method = "over")$data

data_train <- Filter(function(x){(is.factor(x) & length(unique(x))< 20) | !is.factor(x)}, data_train)

View(df_status(data_train))

rfmodel <- randomForest(x = data_train[,2:92], 
                        y = data_train$isFraud, 
                        importance = T,
                        do.trace=TRUE,
                        ntree=100)

importance <- importance(rfmodel)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

varImportanceSorted <- varImportance[order(-varImportance$Importance),]

mostImportantVars <- varImportanceSorted[1:30,]

ggplot(mostImportantVars, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip()

View(table(data_train$P_emaildomain))



# Predicting on train set
predTrain <- predict(rfmodel, data_train, type = "class")
# Checking classification accuracy
table(predTrain, data_train$isFraud)  

# Predicting on test set
predTest <- predict(rfmodel, data_test, type = "class")
# Checking classification accuracy
table(predTest, data_test$isFraud)  

mean(predTest == data_test$isFraud) 


data_train_2 <- data_train[c('isFraud',as.vector(mostImportantVars[,'Variables']))]
data_test_2 <- data_test[c('isFraud',as.vector(mostImportantVars[,'Variables']))]

View(df_status(data_train_2))

rfmodel_2 <- randomForest(x = data_train_2[,2:30], 
                        y = data_train_2$isFraud, 
                        importance = T,
                        do.trace=TRUE,
                        ntree=500)

# Predicting on train set
predTrain_2 <- predict(rfmodel_2, data_train_2, type = "class")
# Checking classification accuracy
table(predTrain_2, data_train_2$isFraud)  
mean(predTrain_2 == data_train_2$isFraud) 

# Predicting on test set
predTest_2 <- predict(rfmodel_2, data_test_2, type = "class")
# Checking classification accuracy
table(predTest_2, data_test_2$isFraud)  
mean(predTest_2 == data_test_2$isFraud) 


importance_2 <- importance(rfmodel_2)

varImportance_2 <- data.frame(Variables = row.names(importance_2), 
                            Importance = round(importance_2[ ,'MeanDecreaseGini'],2))

varImportanceSorted_2 <- varImportance_2[order(-varImportance_2$Importance),]


ggplot(varImportanceSorted_2, aes(x = reorder(Variables, Importance), 
                              y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip()


# submission
data_test_submission <- read.csv(file = "./data/data_test_submission.csv", header = TRUE, sep  = ",")
data_test_submission <- data_test_submission[as.vector(mostImportantVars[,'Variables'])]
data_test_submission_zeros <- data_test_submission
data_test_submission_zeros[is.na(data_test_submission_zeros)] <- 0
data_test_submission_zeros <- data_test_submission_zeros %>% mutate_if(is.factor, fct_explicit_na, na_level = 'Unknown')

data_test_submission_zeros <- data_test_submission_zeros %>%
  mutate(id_35 = as.logical(
    case_when(
      id_35 == 'TRUE' ~ TRUE,
      id_35 == 'FALSE' ~ FALSE,
      TRUE ~ TRUE)))

View(df_status(data_test_submission_zeros))

data_test_submission_zeros$R_emaildomain <- 
  factor(data_test_submission_zeros$R_emaildomain, levels = levels(data_train_2$R_emaildomain))

data_test_submission_zeros$P_emaildomain <- 
  factor(data_test_submission_zeros$P_emaildomain, levels = levels(data_train_2$P_emaildomain))

data_test_submission_zeros$DeviceInfo <- 
  factor(data_test_submission_zeros$DeviceInfo, levels = levels(data_train_2$DeviceInfo))

data_test_submission_zeros$ProductCD <- 
  factor(data_test_submission_zeros$ProductCD, levels = levels(data_train_2$ProductCD))

data_test_submission_zeros$id_35 <- 
  factor(data_test_submission_zeros$id_35, levels = levels(data_train_2$id_35))


predSubmission <- predict(rfmodel_2, data_test_submission_zeros, type = "class")

data_submission <- read.csv(file = "./data/test_innerjoin.csv", header = TRUE, sep  = ",")

data_submission <- data_submission['TransactionID']

predSubmission <- ifelse(predSubmission == 'Yes', 1, 0)

result <- data.frame(TransactionID=data_submission$TransactionID, isFraud=predSubmission)
write.csv(result, file = "solution1.csv", row.names = FALSE)
