library("ROSE")

data_train <- read.csv(file = "./data/data_train.csv", header = TRUE, sep  = ",")
data_test <- read.csv(file = "./data/data_test.csv", header = TRUE, sep  = ",")

data_train <- ovun.sample(isFraud~.,data = data_train, method = "over")$data

data_train <- data_train %>% mutate(isFraud = as.numeric(ifelse(isFraud == 'Yes', 1, 0)))
data_test <- data_test %>% mutate(isFraud = as.numeric(ifelse(isFraud == 'Yes', 1, 0)))


mylogit <- glm(isFraud ~ TransactionAmt + C14 + id_02 + V246 + V243 + V200 + V244 + id_01 + V229
                 , data = data_train, family = "binomial")

predict <- predict(mylogit, data_test, type = 'response')
predict <- ifelse(predict > 0.5, 1, 0)
# confusion matrix
table_mat <- table(data_test$isFraud, predict)
table_mat

data_test
length(predict)
