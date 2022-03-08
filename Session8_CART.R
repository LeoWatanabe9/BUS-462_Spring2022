############################################################
# BUS 462 | Spring 2022 | Session 8 
# A gentle intro to ML : CART
# Titanic Data - CART Classification using TREES
# CK 
# 08 MAR 2022
############################################################


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(pastecs)
library(stargazer)
library(PerformanceAnalytics)
library(dplyr)

#Step 1: Import Data -- up on canvas or online
# data ref:https://data.world/nrippner/titanic-disaster-dataset 
path <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv"
titanic <- fread(path)  

head(titanic)
tail(titanic)
# 

#Step 2: Clean the Data
stat.desc(titanic)  # There's a bunch of NAs

# let's keep only those factors of interest
dt <- titanic[,c("survived","sex","age","sibsp")]

# summ stats
stat.desc(dt)
# notice NA and age and sex are chr
dt$age <- as.integer(dt$age)
dt$sex <- as.factor(dt$sex) # convert sex to factor
dt$sex <- as.numeric(dt$sex) # convert factor to int/numeric

# omit NA's
dt <- na.omit(dt)

#summ stats
stat.desc(dt)
stargazer(dt,type="text")

# correlation plot
chart.Correlation(dt) 

# shuffle the data - randomize rows to prep for splitting into test and train parts
shuffle_index <- sample(1:nrow(dt)) # generates random row #s in a vector
dt <- dt[shuffle_index, ] # use the randomly generated rows to shuffle dt
head(dt)

#Step 3:Create train / Test Data

# create test and train data
# simply take the top 80% for train and bottom 20% for test
n_cut <- round(nrow(dt)*.8,0) # row at which to cut data
data_train <- dt[1:n_cut] # create training data subset
data_test <- dt[(n_cut+1):nrow(dt)] # create test data subset

#check dim
dim(data_train)
dim(data_test)

# test distribution of survivors
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

# Step 4:  BUILD THE MODEL
#install.packages("rpart.plot")	
library(rpart)
library(rpart.plot)
# Prediction Model using CART
fit <- rpart(survived~sex+age+sibsp, data = data_train, method = 'class') 
# plotting the tree!
rpart.plot(fit, extra = 106) # tree plotting

#step 5 Prediction for test data 
predict_unseen <-predict(fit, data_test, type = 'class')

# Testing the passengers who didn't make it and those who did.
# Creating the confusion matrix!
ConfMatrix <- table(data_test$survived, predict_unseen)
ConfMatrix
# Model correctly predicted 128 deaths 
## but classified xx survivors who actually died

#step 6: Prediction Performance Measures
accuracy_Test <- sum(diag(ConfMatrix)) / sum(ConfMatrix)

print(paste('Accuracy for test', accuracy_Test))

## Exercise 1
# Calculate the other Performance Metrics for classification
## Precision
## Recall
## F-1 !


# Exercise 2
# Run CART with other variables included - parch, fare, embarked...
