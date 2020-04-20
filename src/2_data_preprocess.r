library(dplyr)   # gives mutate_if
library(forcats) # gives fct_explicit_na
library(corrplot)
library('caret')
library(funModeling)
library(tidyverse)
library(caret)

# ------------------------- FUNCTIONS ------------------------- #

# Function to convert a range of columns to factor
column_range_to_factor <- function(df, col, range) {
  for(i in range){
    col_name <- paste(col, i, sep = "")
    df[,col_name] <- as.factor(df[,col_name])
  }
  return(df)
}

# Function to remove high correlated columns by pairs
remove_correlated_columns <- function(df, correlation_value) {
  # Get numeric columns
  df_numeric <- df[, unlist(lapply(df, is.numeric))]
  # Get correlation matrix
  cor_matrix <- cor(df_numeric)
  
  hc <- findCorrelation(cor_matrix, cutoff=correlation_value) 
  hc <- sort(hc)
  
  # Get name of columns to remove
  cols_to_remove <- colnames(df_numeric[, c(hc)])
  
  # Return dataframe without columns to remove
  return(df[ , !names(df) %in% cols_to_remove])
}

# Function to make a basic preprocessing in a dataframe
basic_preprocessing <- function(df) {
  # Convert to factor
  df <- df %>%
    column_range_to_factor('card', 1:6) %>%
    column_range_to_factor('addr', 1:2) %>%
    column_range_to_factor('M', 1:9) %>%
    column_range_to_factor('id_', 12:38)
  
  # Remove TransactionID and TransactionDT
  df <- subset(df, select = -c(TransactionID, TransactionDT))
  # Remove columns with more than 40% of na values
  df <- df[, which(colMeans(is.na(df)) <= 0.4)]
  # Replace na values in factor with 'Unknown'
  df <- df %>% mutate_if(is.factor, fct_explicit_na, na_level = 'Unknown')
  # Remove rows with na values
  df <- na.omit(df)
  # Remove columns with one single value
  df <- Filter(function(x) length(unique(x))>1, df)
  # Remove columns with high correlation between them 
  df <- remove_correlated_columns(df, 0.9)
  
  # Group features with similar string
  df <- df %>%
    mutate(id_31 = as.factor(
      case_when(
        grepl('chrome', id_31) ~ 'chrome',
        grepl('safari', id_31) ~ 'safari',
        grepl('edge', id_31) ~ 'edge',
        grepl('firefox', id_31) ~ 'firefox',
        grepl('ie', id_31) ~ 'ie',
        grepl('samsung', id_31) ~ 'samsung',
        TRUE ~ 'other'))) %>%
    mutate(DeviceInfo = as.factor(
      case_when(
        grepl('Windows', DeviceInfo) ~ 'Windows',
        grepl('iOS Device', DeviceInfo) ~ 'iOS Device',
        grepl('Unknown', DeviceInfo) ~ 'Unknown',
        grepl('MacOS', DeviceInfo) ~ 'MacOS',
        grepl('Trident', DeviceInfo) ~ 'Trident',
        grepl('SM', DeviceInfo) ~ 'Samsung',
        TRUE ~ 'Other'))) %>%
    mutate(P_emaildomain = as.factor(
      case_when(
        grepl('gmail', P_emaildomain) ~ 'gmail',
        grepl('hotmail', P_emaildomain) ~ 'hotmail',
        grepl('anonymous', P_emaildomain) ~ 'anonymous',
        grepl('Unknown', P_emaildomain) ~ 'Unknown',
        grepl('aol', P_emaildomain) ~ 'aol',
        grepl('outlook', P_emaildomain) ~ 'outlook',
        grepl('msn', P_emaildomain) ~ 'msn',
        grepl('live', P_emaildomain) ~ 'live',
        TRUE ~ 'Other'))) %>%
    mutate(R_emaildomain = as.factor(
      case_when(
        grepl('gmail', R_emaildomain) ~ 'gmail',
        grepl('hotmail', R_emaildomain) ~ 'hotmail',
        grepl('anonymous', R_emaildomain) ~ 'anonymous',
        grepl('Unknown', R_emaildomain) ~ 'Unknown',
        grepl('aol', R_emaildomain) ~ 'aol',
        grepl('outlook', R_emaildomain) ~ 'outlook',
        grepl('msn', R_emaildomain) ~ 'msn',
        grepl('live', R_emaildomain) ~ 'live',
        TRUE ~ 'Other')))
  
  # Remove rows with transactionAmt greater than 250 to remove outliers
  df <- subset(df, TransactionAmt <= 250)
  
  # Remove columns with high percentage of zeros
  my_df_status <- df_status(df)
  vars_to_remove=subset(my_df_status, my_df_status$p_zeros > 90)  
  df <- df[, !(names(df) %in% vars_to_remove[,"variable"])]
  
  return(df)
}

test_preprocessing <- function(df) {
  # Convert to factor
  df <- df %>%
    column_range_to_factor('card', 1:6) %>%
    column_range_to_factor('addr', 1:2) %>%
    column_range_to_factor('M', 1:9) %>%
    column_range_to_factor('id_', 12:38)
  
  # Replace na values in factor with 'Unknown'
  df <- df %>% mutate_if(is.factor, fct_explicit_na, na_level = 'Unknown')
  
  # Group features with similar string
  df <- df %>%
    mutate(id_31 = as.factor(
      case_when(
        grepl('chrome', id_31) ~ 'chrome',
        grepl('safari', id_31) ~ 'safari',
        grepl('edge', id_31) ~ 'edge',
        grepl('firefox', id_31) ~ 'firefox',
        grepl('ie', id_31) ~ 'ie',
        grepl('samsung', id_31) ~ 'samsung',
        TRUE ~ 'other'))) %>%
    mutate(DeviceInfo = as.factor(
      case_when(
        grepl('Windows', DeviceInfo) ~ 'Windows',
        grepl('iOS Device', DeviceInfo) ~ 'iOS Device',
        grepl('Unknown', DeviceInfo) ~ 'Unknown',
        grepl('MacOS', DeviceInfo) ~ 'MacOS',
        grepl('Trident', DeviceInfo) ~ 'Trident',
        grepl('SM', DeviceInfo) ~ 'Samsung',
        TRUE ~ 'Other'))) %>%
    mutate(P_emaildomain = as.factor(
      case_when(
        grepl('gmail', P_emaildomain) ~ 'gmail',
        grepl('hotmail', P_emaildomain) ~ 'hotmail',
        grepl('anonymous', P_emaildomain) ~ 'anonymous',
        grepl('Unknown', P_emaildomain) ~ 'Unknown',
        grepl('aol', P_emaildomain) ~ 'aol',
        grepl('outlook', P_emaildomain) ~ 'outlook',
        grepl('msn', P_emaildomain) ~ 'msn',
        grepl('live', P_emaildomain) ~ 'live',
        TRUE ~ 'Other'))) %>%
    mutate(R_emaildomain = as.factor(
      case_when(
        grepl('gmail', R_emaildomain) ~ 'gmail',
        grepl('hotmail', R_emaildomain) ~ 'hotmail',
        grepl('anonymous', R_emaildomain) ~ 'anonymous',
        grepl('Unknown', R_emaildomain) ~ 'Unknown',
        grepl('aol', R_emaildomain) ~ 'aol',
        grepl('outlook', R_emaildomain) ~ 'outlook',
        grepl('msn', R_emaildomain) ~ 'msn',
        grepl('live', R_emaildomain) ~ 'live',
        TRUE ~ 'Other')))
  
  return(df)
}

# ------------------------------------------------------------- #

# Read Data
df_train <- read.csv(file = "./data/train_innerjoin.csv", header = TRUE, sep  = ",")

# Conver isFraud to factor from 0-1 to No-Yes
df_train <- df_train %>% mutate(isFraud = as.factor(ifelse(isFraud == 1, 'Yes', 'No')))

# Preprocessing data
df_train <- basic_preprocessing(df_train)

# Save preprocessed data
write.csv(df_train, "./data/data_train_preprocessing.csv", row.names = FALSE)

# ------------------------------------------------------------- #

# Test submission
df_test <- read.csv(file = "./data/test_innerjoin.csv", header = TRUE, sep  = ",")

# Rename id.01-id.38 to id_01-id_38 in test data
df_test <- df_test %>% rename_at(vars(paste('id.0', 1:9, sep="")), ~ paste('id_0', 1:9, sep=""))
df_test <- df_test %>% rename_at(vars(paste('id', 10:38, sep=".")), ~ paste('id', 10:38, sep="_"))

# Preprocessing data
df_test <- test_preprocessing(df_test)

# Select the same columns of df_train
df_test <- df_test[colnames(df_train)[2:99]]

# Save preprocessed data
write.csv(df_test, "./data/data_test_preprocessing.csv", row.names = FALSE)


