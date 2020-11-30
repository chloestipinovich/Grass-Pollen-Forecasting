# Title       : Random Forest Functions
# Last edited : November 2020

# cleanData takes the raw data and returns 
# data having removed unwanted variables and 
# changed variable names
cleanData = function(data){
  data      = dplyr::select(data, -File.Name, -Dataset, -aid, -Variance, -visibility, -X, -date,-Range, -logvalue, -Count) %>%
    rename(VI_minimum = Minimum, VI_maximum = Maximum,
           VI_mean = Mean, VI_sd = Standard.Deviation,
           VI_UQ = Upper.Quartile,VI_U1.5.IQR = Upper.1.5.IQR, VI_L1.5.IQR = Lower.1.5.IQR,
           VI_LQ = Lower.Quartile, temp = Temperature, dewPoint = Dew.Point)
  data$pollen_cat = as.ordered(data$pollen_cat)
  data$season     = as.factor(data$season)
  return(data)
}

# defineSets takes of three parameters
# trainYears a vector of charaters of length 2 to 4
# valYear a charaters of length 1
# testYear a charaters of length 1
defineSets = function(trainYears, valYear, testYear){
  if (length(trainYears)==2){
    train      = data[which(data$fyear==trainYears[1]|data$fyear==trainYears[2]),]
  }
  if (length(trainYears)==3){
    train      = data[which(data$fyear==trainYears[1]|data$fyear==trainYears[2]|data$fyear==trainYears[3]),]
  }
  if (length(trainYears)==4){
    train      = data[which(data$fyear==trainYears[1]|data$fyear==trainYears[2]|data$fyear==trainYears[3]|data$fyear==trainYears[4]),]
  }
  validation = data[which(data$fyear == valYear),]
  test       = data[which(data$fyear == testYear),]
  return(list(train = train, validation = validation, test = test))
}

# brierProbs fills in the brierData matrix 
# with the appropriate probability distribution data
brierProbs = function(brierData, probData){
  brierData[,1]  = probData[,5]
  brierData[,2]  = probData[,2]
  brierData[,3]  = probData[,3]
  brierData[,4]  = probData[,1]
  brierData[,5]  = probData[,4]
  return(brierData)
}

# brierDataFnc creates the brier data matrix
# and sets values to 1 on the day a particular category occured
brierDataFnc   = function(validation){
  brierData      = matrix(0, ncol = 10, nrow = nrow(validation))
  y              = as.factor(validation$pollen_cat)
  for (i in 1:length(y)){
    if (y[i]=="Very low"){
      brierData[i,6] = 1
    }
    if (y[i]=="Low"){
      brierData[i,7] = 1
    }
    if (y[i]=="Moderate"){
      brierData[i,8] = 1
    }
    if (y[i]=="High"){
      brierData[i,9] = 1
    }
    if (y[i]=="Very high"){
      brierData[i,10] = 1
    }
  }
  return(brierData)
}



# MSdata creates MSE and MAE data frame
# used in the calculation of MSE and MAE
MSdata  = function(validation, yhat){
  observed    = validation %>% mutate(level = case_when(
    pollen_cat == "Very low" ~ 1,
    pollen_cat == "Low" ~ 2,
    pollen_cat == "Moderate" ~ 3,
    pollen_cat == "High" ~ 4,
    pollen_cat == "Very high" ~ 5))
  observed    = as.factor(observed$level)
  
  M1yhatDF    = as.data.frame(as.character(yhat))
  names(M1yhatDF) = "pollen_cat"
  predicted   =  M1yhatDF %>% mutate(level = case_when(
    pollen_cat == "Very low" ~ 1,
    pollen_cat == "Low" ~ 2,
    pollen_cat == "Moderate" ~ 3,
    pollen_cat == "High" ~ 4,
    pollen_cat == "Very high" ~ 5) )
  predicted = as.factor(predicted$level)
  return(list("observed"=observed, "predicted"=predicted))
}

# brier_score calculates and returns the brier score
brier_score = function(df){
  brier_score = 0 # set to start at zero
  for(i in 1:nrow(df)){ # iterate over the rows
    day_score = 0 # for each day's contribution, start at 0
    for(j in 1:5){
      day_score = day_score + (df[i, j] - df[i, j+5])^2
    }
    brier_score = brier_score + day_score
  }
  return(brier_score/nrow(df))
}

# mse calculates and returns the MSE
mse = function(cm){
  mse = 0
  for(r in 1:5){
    for(c in 1:5){
      mse = mse + cm[r, c]* (r - c)^2
    }
  }
  return(mse/sum(cm))
}

# mae calculates and returns the MAE
mae = function(cm){
  mae = 0
  for(r in 1:5){
    for(c in 1:5){
      mae = mae + cm[r, c]* abs(r - c)
    }
  }
  return(mae/sum(cm))
}

# setGrid defines the grid search range as per model building
# type can take on values of 1 or 2 as appropriate
setGrid = function(type){
  if (type == 1){
    rf_grid = expand.grid(mtry = 2:9,
                           splitrule = 'gini',
                           min.node.size = c(5,10,20)) 
  }
  else{
    rf_grid = expand.grid(mtry = 2:6,
                           splitrule = 'gini',
                           min.node.size = c(5,10,20)) 
    ctrl = trainControl(method = 'oob', verboseIter = T)
  }
  return(rf_grid)
}


# dataManipulation returns a training or validation set including
# the appropriate variables per data set
dataManipulation = function(M, dataSet){
  if (M == 1){
    data = dplyr::select(dataSet, -pollen_count, -fyear) %>% na.omit()
  }
  if (M == 2){
    data <- dataSet %>%
      mutate(cat_lag1  = lag(pollen_cat, 1),cat_lag2 = lag(pollen_cat, 2),cat_lag3 = lag(pollen_cat, 3),cat_lag4 = lag(pollen_cat, 4),cat_lag5 = lag(pollen_cat, 5),
            count_lag1 = lag(pollen_count, 1),count_lag2 = lag(pollen_count, 2),count_lag3 = lag(pollen_count, 3),count_lag4 = lag(pollen_count, 4),count_lag5 = lag(pollen_count, 5),
            temp_lag1  = lag(temp, 1),temp_lag2 = lag(temp, 2),temp_lag3 = lag(temp, 3),
            dew_point_lag1    = lag(dewPoint, 1), dew_point_lag2    = lag(dewPoint, 2), dew_point_lag3    = lag(dewPoint, 3),
            maxtemp_lag1      = lag(max_temp, 1), maxtemp_lag2      = lag(max_temp, 2), 
            maxtemp_lag3      = lag(max_temp, 3), maxtemp_anom_lag1 = lag(maxtemp_anom, 1), maxtemp_anom_lag2 = lag(maxtemp_anom, 2),maxtemp_anom_lag3 = lag(maxtemp_anom, 3),
            humid_lag1        = lag(humid, 1),humid_lag2        = lag(humid, 2),humid_lag3        = lag(humid, 3)) %>%
      select(-pollen_count) %>% na.omit()
  }
  if (M == 3){
    data <- select(dataSet, -VI_minimum, -VI_maximum, -VI_mean, -VI_sd, 
                     -VI_UQ, -VI_U1.5.IQR, -veg_index, -VI_L1.5.IQR, VI_LQ, 
                     - min_temp, -max_temp, -temp, -dewPoint, 
                     -humid, -rain, -wind_speed, -wind_dir, -VI_LQ,  
                     -day, -month, -year, -fyear, -season, -mintemp_anom, -maxtemp_anom) %>%
      mutate(count_lag1 = lag(pollen_count, 1), count_lag2 = lag(pollen_count, 2), count_lag3 = lag(pollen_count, 3), count_lag4 = lag(pollen_count, 4), count_lag5 = lag(pollen_count, 5)) %>%
      select(-pollen_count) %>% na.omit()
  }
  if (M == 4){
    data <- select(dataSet, -VI_minimum, -VI_maximum, -VI_L1.5.IQR, -rain,
                     -year, -fyear) %>%
      mutate(count_lag1 = lag(pollen_count, 1), count_lag2 = lag(pollen_count, 2), count_lag3 = lag(pollen_count, 3), count_lag4 = lag(pollen_count, 4), count_lag5 = lag(pollen_count, 5),
             temp_lag1         = lag(temp, 1), temp_lag2         = lag(temp, 2), temp_lag3         = lag(temp, 3),
             dew_point_lag1    = lag(dewPoint, 1), dew_point_lag2    = lag(dewPoint, 2), dew_point_lag3    = lag(dewPoint, 3),
             maxtemp_lag1      = lag(max_temp, 1), maxtemp_lag2      = lag(max_temp, 2), maxtemp_lag3      = lag(max_temp, 3),
             maxtemp_anom_lag1 = lag(maxtemp_anom, 1), maxtemp_anom_lag2 = lag(maxtemp_anom, 2), maxtemp_anom_lag3 = lag(maxtemp_anom, 3),
             humid_lag1        = lag(humid, 1), humid_lag2        = lag(humid, 2), humid_lag3        = lag(humid, 3)) %>%
      select(-pollen_count) %>% na.omit()
  }
  return(data)
}

# rfModel builds and returns the optimal random forest 
# given input data and the grid range to search over
rfModel = function(data, rf_grid){
  hyperParams = train(pollen_cat ~ ., 
                  data = data,
                  method = 'ranger',
                  num.trees = 2000,
                  verbose = T,
                  set.seed(2020),
                  importance = 'impurity',
                  trControl = ctrl,
                  tuneGrid = rf_grid)
  rf = randomForest(pollen_cat~., 
                      data = data,
                      mtry = as.numeric(hyperParams$finalModel[4]),
                      min.node.size = as.numeric(hyperParams$finalModel[5]),
                      importance = TRUE,
                      do.trace = 500,
                      ntree = 2000,
                      set.seed(2020))
  return(rf)
}

# metricFnct calculates all metrics for a particular period
metricFnct = function(season, validation, rf, M){
  val = dataManipulation(M, validation)
  val = na.omit(val)
  if (season == 2){
    In         = which(validation$season == "In season")
    val = na.omit(val[In,])
  }
  if (season == 3){
    Out = which(validation$season == "Not in season")
    val = na.omit(val[Out,])
  }
  
  y          = factor(val$pollen_cat, levels = c("Very low", "Low", "Moderate", "High", "Very high"))
  yhat       = factor(predict(rf, val), levels = c("Very low", "Low", "Moderate", "High", "Very high"))
  probs      = predict(rf, val, "prob")
  brierData  = brierDataFnc(val)
  brierData  = brierProbs(brierData, probs)
  MSdata     = MSdata(val,yhat)
  CMtable    = confusionMatrix(y, yhat)$table

  accuracy   = confusionMatrix(y, yhat)$overall['Accuracy']
  MSE        = mse(CMtable)
  MAE        = mae(CMtable)
  brier      = brier_score(brierData)
  return(list(accuracy = accuracy, MSE = MSE, MAE = MAE, brier = brier))
}
