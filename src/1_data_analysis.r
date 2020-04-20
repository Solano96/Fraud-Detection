library(funModeling)
library(ggplot2)
library(cowplot)
library(dplyr)

# Read Data
df <- read.csv(file = "./data/train_innerjoin.csv", header = TRUE, sep  = ",")

# See dataset dimesion
dim(df)

# Delete TransactionID and check if exists duplicated
df <- subset(df, select = -TransactionID)
sum(duplicated(df))

# Summary of zeros and missing values
df_status(df)

# Conver isFraud to factor from 0-1 to No-Yes
df <- df %>% mutate(isFraud = as.factor(ifelse(isFraud == 1, 'Yes', 'No')))

# Fraud proportion
summary(df$isFraud)
ggplot(df, aes(x=isFraud, color=isFraud, fill=isFraud)) +
  geom_bar()


View(df_status(df))


fraud_no <- subset(df, isFraud == 'No')
fraud_yes <- subset(df, isFraud == 'Yes')


# -------------------- TransactionAmt --------------------

ggplot(df, aes(x=isFraud, y=TransactionAmt)) +
  geom_boxplot()

df_filter <- subset(df, TransactionAmt < 250)

ggplot(df_filter, aes(x=isFraud, y=TransactionAmt)) +
  geom_boxplot()


# Factor Levels
summary(df$P_emaildomain)
summary(df$id_31)
summary(df$DeviceInfo)



# -------------------- Card3 --------------------

histogram_card3_no <- ggplot(fraud_no, aes(x=card3)) +
  geom_histogram(position="identity")

histogram_card3_yes <- ggplot(fraud_yes, aes(x=card3)) +
  geom_histogram(position="identity")

plot_grid(histogram_card3_no, histogram_card3_yes)


# -------------------- Card6 --------------------

histogram_card6_no <- ggplot(fraud_no, aes(x=card6)) +
  geom_bar() +
  ggtitle("card6 no fraud")

histogram_card6_yes <- ggplot(fraud_yes, aes(x=card6)) +
  geom_bar() +
  ggtitle("card6 fraud")

plot_grid(histogram_card6_no, histogram_card6_yes)


# -------------------- C3 --------------------

histogram_C3_no <- ggplot(fraud_no, aes(x=C3)) +
  geom_bar() +
  ggtitle("C3 no fraud")

histogram_C3_yes <- ggplot(fraud_yes, aes(x=C3)) +
  geom_bar() +
  ggtitle("C3 fraud")

plot_grid(histogram_C3_no, histogram_C3_yes)


# Plot correlation of some variables

# Remove columns with more than 40% of na values
df <- df[, which(colMeans(is.na(df)) <= 0.4)]
# Replace na values in factor with 'Unknown'
df <- df %>% mutate_if(is.factor, fct_explicit_na, na_level = 'Unknown')
# Remove rows with na values
df <- na.omit(df)
df <- Filter(function(x) length(unique(x))>1, df)
corrplot(cor(df[paste("V", 200:250, sep="")]))
