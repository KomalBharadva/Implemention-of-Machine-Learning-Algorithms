setwd("D:/NCI/DMML1/Project/Datasets")
getwd()

# Loading required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(ggmap)
library(tidyverse)
library(caTools)
library(car)
library(GGally)
library(randomForest)

## Reading CSV File
House_df <- read.csv("House_data.csv", header=T, stringsAsFactors = T)

dim(House_df) # Checking dimension which is number of rows and columns
names(House_df) # Checking 
class(House_df) # This shows the class or type of object
str(House_df)  # This tells us about the structure of dataframe
summary(House_df) # This provides statistics summary of dataframe
head(House_df) # To see first few rows of dataframe

## Data Cleaning
# Checking for missing values column wise
sapply(House_df, function(x) sum(is.na(x))) 

# Changing date column to yyyymm format
House_df$date = substr(House_df$date, 1, 6)
# Converting date to numeric as we can only use numeric values for correlation
House_df$date = as.numeric(as.character(House_df$date))

# Checking the distribution of price.
hist(House_df$price)

# It seems to be right skewed. Lets make it uniform by taking the log.
House_df$price = log(House_df$price)

# Checking the distribution of price after taking log of price variable
hist(House_df$price)

summary(House_df)
# We can see a outlier in number of bedrooms i.e. 33, so we remove it.
House_df = House_df[House_df$bedrooms <= 10, ]
House_df$bedrooms = as.factor(House_df$bedrooms)

House_df$floors = as.factor(House_df$floors)
House_df$waterfront = as.factor(House_df$waterfront)
House_df$view = as.factor(House_df$view)
House_df$condition = as.factor(House_df$condition)

# It looks like most of the houses do not have basement. Lets check
length(House_df$sqft_basement[House_df$sqft_basement == 0])
# 13126 houses do not have basement. 

# Let's divide this into 2 parts: 0 who do not have basement and 1 have.
House_df$sqft_basement[House_df$sqft_basement != 0] = 1
House_df$sqft_basement = as.factor(House_df$sqft_basement)

length(House_df$yr_renovated[House_df$yr_renovated == 0])
# 20698 houses have not renovated 

# Let's divide this into 2 parts: 0 who do not have renovated and 1 have.
House_df$yr_renovated[House_df$yr_renovated != 0] = 1
House_df$yr_renovated = as.factor(House_df$yr_renovated)

House_df$zipcode = as.factor(House_df$zipcode)

# There seems to be outliers in sqft_living. So we remove this.
House_df = House_df[House_df$sqft_living != 13540, ]

ggcorr(House_df %>% select(-date), name = "corr", label = TRUE, hjust = 1, 
       label_size = 2.5, angle = -45, size = 3)

  # We can see many variables has weak or no correlation with prices so we drop them as they will not contribute significantly in prediction.
House_df <- subset( House_df, select = -c(id, date, sqft_lot, yr_built, lat, long, sqft_above, sqft_lot15))

# Feature scaling
numericFeatures = sapply(House_df[, -1], is.numeric)
numericFeatures = c(FALSE, numericFeatures) # No need to scale price
House_df[numericFeatures] = sapply(House_df[numericFeatures], scale)

### Linear Regression:
# Splitting the dataset into the Training set and Test set for Linear Regression
set.seed(123)
split = sample.split(House_df$price, SplitRatio = 0.7)
train_lm = subset(House_df, split == TRUE)
test_lm = subset(House_df, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
mlregressor = lm(formula = price ~ .,
                 data = train_lm)
summary(mlregressor)
plot(mlregressor)

# To check the Variance Inflation factor
mymodel <- lm(price~.,data = train_lm)
vif(mymodel)

#Now we predict the test results
test_lm$pred_lm = predict(mlregressor, newdata = test_lm)
res_lm <- test_lm$pred_lm - test_lm$price
rmse_lm <- sqrt(mean(res_lm^2))
rmse_lm

House_df_rf <- subset(House_df, select = -c(zipcode))

## Random Forest
# Splitting the data into train and test sets.
set.seed(1234)
id <- sample(2, nrow(House_df_rf), prob = c(0.70,0.30), replace = TRUE)
train_rf <- House_df_rf[id==1,]
test_rf <- House_df_rf[id==2,]
str(train_rf)

# Model fit with Random Forest
model_rf <- randomForest(price ~ ., train_rf[,1:12], mtry=6, importance=TRUE)
model_rf
summary(model_rf)
importance(model_rf)
varImpPlot(model_rf,type=2)

# Predict with rf model
test_rf$pred <- predict(model_rf, test_rf)
res_rf <- test_rf$pred-test_rf$price
rmse_rf <- sqrt(mean(res_rf^2))
rmse_rf

