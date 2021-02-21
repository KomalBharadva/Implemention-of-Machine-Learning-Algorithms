setwd("D:/NCI/DMML1/Project/Datasets")
getwd()

library(dplyr)
library(ggplot2)
library(corrplot)

fraud_data <-  read.csv("Fraud_data.csv", header = TRUE, stringsAsFactors = TRUE)

dim(fraud_data) # Checking dimension which is number of rows and columns
str(fraud_data)  # This tells us about the structure of dataframe
summary(fraud_data) # This provides statistics summary of dataframe
head(fraud_data) # To see first few rows of dataframe

# Checking the distribution of target variable
ggplot(fraud_data,aes(x=isFraud,fill=isFraud))+geom_bar(stat = 'count')+
  labs(x = '0 and 1') + geom_label(stat='count',aes(label=..count..), size=5)+ 
  theme_grey(base_size = 11)

# Checking for missing values column wise
sapply(fraud_data, function(x) sum(is.na(x))) 

## EDA-1
ggplot(data = fraud_data, aes(x = type , fill = type)) + geom_bar() + 
  labs(title = "Transactions as per Type",  x = 'Transaction Type' , 
       y = 'No of transactions' ) +theme_classic()

## EDA-2
# Transaction Types which are most probably to be Fraud
Fraud_type <- fraud_data %>% group_by(type) %>% summarise(fraud_trans = sum(isFraud))
ggplot(data = Fraud_type, aes(x = type,  y = fraud_trans)) + 
  geom_col(aes(fill = 'type'), show.legend = FALSE) + 
  labs(title = 'Fraud transactions as Per type', x = 'Transaction type', 
       y = 'No of Fraud Transactions') + geom_text(aes(label = fraud_trans), 
                                                   size = 4, hjust = 0.5) + theme_classic()

## EDA-3
# Checking correlations
corr <- sapply(fraud_data, is.numeric)
corrchart <- cor(fraud_data[,corr])
corrplot(corrchart, main = '\n\n Correlation Chart',method = "number")
#Not seeing much of correlation for is fraud

# Eliminating redundant columns
fraud_data <- subset(fraud_data, select = -c(nameOrig, nameDest, isFlaggedFraud))

## Feature Engg
fraud_data$balanceOrg = ifelse(fraud_data$newbalanceOrig - fraud_data$oldbalanceOrg > 0,'Positive',
                               ifelse(fraud_data$newbalanceOrig - fraud_data$oldbalanceOrg < 0,"Negative","Zero"))
fraud_data$balanceOrg <-as.factor(fraud_data$balanceOrg)

fraud_data$balanceDest<-ifelse(fraud_data$newbalanceDest - fraud_data$oldbalanceDest > 0,'Positive',
                               ifelse(fraud_data$newbalanceDest - fraud_data$oldbalanceDest < 0,"Negative","Zero"))
fraud_data$balanceDest<-as.factor(fraud_data$balanceDest)

dim(fraud_data)

# In this dataset we can see some columns are not required so we drop them.
fraud_data <- subset(fraud_data, select = -c(oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest))

# Converting Categorical columns to numeric
library(fastDummies)
fraud_data <- dummy_cols(fraud_data)

fraud_data$isFraud <- as.factor(fraud_data$isFraud)

#Dropping redundant columns
fraud_data <- subset(fraud_data, select = -c(type, balanceOrg, balanceDest))

#fraud transactions are either CASH_OUT or TRANSFER type so dropping others.
fraud_data <- subset(fraud_data, select = -c(type_DEBIT, type_PAYMENT, type_CASH_IN))
dim(fraud_data)
write.csv(fraud_data, file = "fraud_final.csv", row.names = FALSE)
