# Title       : Random Forest Models
# Last edited : November 2020
# Requires    : random_forest_functions.R

# Clear environment

rm(list = ls())

# Load Required Libraries 

{
  library(tidyverse)
  library(ranger)
  library(caret)
  library(mgcv)
  library(gbm)
  library(reshape2)
  library(dplyr)
  library(tidyverse)
  library(randomForest)
  library(skimr)
  library(corrplot)
}

# Step 1: set working directory

setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen")

# Step 2: load data_complete.csv

data      = as.data.frame(read.csv("Final_Data/data_complete.csv"))

# Step 3: call random_forest_functions.R using source() to use function
source("randomForests/random_forest_functions.R")

# Step 4: Clean data, set training, validation, and test sets 

data       = cleanData(data)
trainYears = c("2011", "2012") # Can only take on a list of length 2:4
valYear    = "2013"            # Must be a single year
testYear   = "2014"            # Must be a single year
sets       = defineSets(trainYears, valYear, testYear)
train      = sets$train; validation = sets$validation; test = sets$validation

# Step 5: Select model
# M can take on values 1 to 4
M = 4

# Step 6: Set grid search range using setGrid() function
ctrl    = trainControl(method = 'oob', verboseIter = T)

if (M==3){
  # setGrid() can take on the following parameters:
  # 1 = mtry 2:9, min.node.size = 5,10,20
  # 2 = mtry 2:6, min.node.size = 5,10,20
  rf_grid = setGrid(2)
}
if (M!=3){
  # setGrid() can take on the following parameters:
  # 1 = mtry 2:9, min.node.size = 5,10,20
  # 2 = mtry 2:6, min.node.size = 5,10,20
  rf_grid = setGrid(1)
}

# Step 7: Define input data as per model
predictors = dataManipulation(M, train)
names(predictors)

# Step 7: Make random forest model
rf = rfModel(predictors, rf_grid)

# Step 8: Calculate metrics
# Season can take on 3 values
# 1 = Total 
# 2 = In Season
# 3 = Out of Season
season  = 3
(metrics = metricFnct(season,validation, rf, M))






