library(randomForest)
library(ggplot2)
library(funModeling)
library("ROSE")

# Read data
df <- read.csv(file = "./data/data_train_preprocessing.csv", header = TRUE, sep  = ",")

# Check class proportion before oversampling
as.data.frame(table(df$isFraud))

# Oversampling
df <- ovun.sample(isFraud~.,data = df, method = "over")$data

# Check class proportion after oversampling
as.data.frame(table(df$isFraud))

# Filter categorical variables with high diversity (greater than 20)
df <- Filter(function(x){(is.factor(x) & length(unique(x))< 20) | !is.factor(x)}, df)

rfmodel <- randomForest(x = df[,2:92], 
                        y = df$isFraud, 
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

df_2 <- df[c('isFraud',as.vector(mostImportantVars[,'Variables']))]
write.csv(df_2, "./data/data_train_preprocessing_2.csv", row.names = FALSE)

# ---------------------------------------------------------------------# 

# Create an index to split data keeping distribution class
train.index <- createDataPartition(df_2$isFraud, p = .7, list = FALSE)

data_train <- df_2[ train.index,]
data_test  <- df_2[-train.index,]

data_train <- ovun.sample(isFraud~.,data = data_train, method = "over")$data

rfmodel_2 <- randomForest(x = data_train[,2:31], 
                          y = data_train$isFraud, 
                          importance = T,
                          do.trace=TRUE,
                          ntree=200)

# Predicting on train set
predTrain_2 <- predict(rfmodel_2, data_train, type = "class")
# Checking classification accuracy
confusionMatrix(table(predTrain_2, data_train$isFraud)) 
mean(predTrain_2 == data_train_2$isFraud) 

# Predicting on test set
predTest_2 <- predict(rfmodel_2, data_test, type = "class")
# Checking classification accuracy
table(data_test$isFraud, predTest_2)
mean(predTest_2 == data_test$isFraud) 
