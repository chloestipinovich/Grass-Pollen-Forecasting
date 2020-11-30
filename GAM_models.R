# GAM models and testing
# Libraries
{
  library(tidyverse)
  library(mgcv)
}

rm(list = ls())

# Functions
# For testing we need a brier score function
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

# MSE and MAE functions
mse = function(cm){
  mse = 0
  for(r in 1:5){
    for(c in 1:5){
      mse = mse + cm[r, c]* (r - c)^2
    }
  }
  return(mse/sum(cm))
}

# MAE
mae = function(cm){
  mae = 0
  for(r in 1:5){
    for(c in 1:5){
      mae = mae + cm[r, c]* abs(r - c)
    }
  }
  return(mae/sum(cm))
}

# EMA function -----------
# Code adapted from Yu (2020) 
# https://bookdown.org/kochiuyu/technical-
# analysis-with-r-second-edition/exponential-moving-average-ema.html
EMA = function (series, n){
  ema = c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(series[1:n])
  betas = c()
  beta <- 0.3
  for (i in (n+1):length(series)){
    ema[i] <- beta * series[i] + 
      (1-beta) * ema[i-1]
  }
  return(ema)
}


# Read in data
# setwd("/Users/skycope/Documents/GitHub/grass-pollen")
total   = read.csv("Final_Data/data_complete.csv", h = T) %>% 
  mutate(date = as.Date(date))

# set the training year
train = filter(total, fyear < 2014)
# moving average variables --------------
ma = train %>% 
  select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
         fyear, season, pollen_cat, visibility, Dew.Point) %>%
  mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
         #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
         rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
         rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
         rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
         rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
         rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1),
         rollmean_visibility = lag(rollmean(visibility, 7, na.pad = T, align = 'right'), 1),
         rollmean_dewpoint = lag(rollmean(Dew.Point, 7, na.pad = T, align = 'right'), 1))

# exponential moving average variables
ema = train %>% 
  select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
         wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
         Dew.Point) %>% 
  mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
         #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
         rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
         rollmean_rain      = lag(EMA(rain, 7), 1),
         rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
         rollmean_humid =  lag(EMA(humid, 7), 1),
         rollmean_winddir = lag(EMA(wind_dir, 7), 1),
         rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 

# Exponential moving average model ---------
# Also referred to as Model 2.1
ema_1 = gam(pollen_count ~ 
              s(max_temp) + s(rollmean_maxtemp) + 
              s(rollmean_pollen) + s(min_temp) +
              humid + 
              s(rain) + s(rollmean_rain) +
              s(wind_speed) + s(rollmean_windspeed) +
              veg_index + 
              s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
              s(ds, bs = 'cc', by = fyear) +
              s(Dew.Point) + 
              visibility, data = ema,
            family = nb(), scale = -0.1, select= TRUE)

(theta_est <- ema_1$family$getTheta(TRUE))

# With theta est
ema_2 = gam(pollen_count ~ 
              s(max_temp) + s(rollmean_maxtemp) + 
              s(rollmean_pollen) + s(min_temp) +
              s(humid) + 
              s(rain) + s(rollmean_rain) +
              s(wind_speed) + s(rollmean_windspeed) +
              veg_index + 
              s(wind_dir, bs = 'cc') + 
              s(rollmean_winddir, bs = 'cc') +
              s(ds, bs = 'cc', by = fyear) +
              s(Dew.Point) + 
              s(visibility), data = ema,
            family = negbin(theta_est), select = TRUE)

# Moving average model
# Also referred to as Model 2.2
ma_1 = gam(pollen_count ~ 
             s(max_temp) + s(rollmean_maxtemp) + 
             s(rollmean_pollen) + s(min_temp)  +
             s(humid) + 
             s(rain) + s(rollmean_rain) +
             s(wind_speed) + s(rollmean_windspeed) +
             veg_index + 
             s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
             s(ds, bs = 'cc', by = fyear) +
             s(Dew.Point) + 
             s(visibility), data = ma,
           family = nb(), scale = -0.1, select = TRUE)

(theta_est_2 <- ma_1$family$getTheta(TRUE))

# With theta est
ma_2 = gam(pollen_count ~ 
             s(max_temp) + s(rollmean_maxtemp) + 
             s(rollmean_pollen) + s(min_temp) +
             humid + 
             s(rain) + s(rollmean_rain) +
             s(wind_speed) + s(rollmean_windspeed) +
             veg_index + 
             s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
             s(ds, bs = 'cc', by = fyear) +
             s(Dew.Point) + 
             visibility, data = ma,
           family = negbin(theta_est_2), select = TRUE)

# Simple model, with fewer covariates:
# Also referred to as Model 2.3
simple = gam(pollen_count ~ 
               s(max_temp) +
               rollmean_pollen +
               s(ds, bs = 'cc') + 
               s(rain) + s(rollmean_rain) + s(veg_index), data = ema,
             family = nb(), scale = -0.1, select = TRUE)

(theta_est_3 <- simple$family$getTheta(TRUE))

# With theta est
simple_2 = gam(pollen_count ~ 
                 s(max_temp) +
                 rollmean_pollen +
                 s(ds, bs = 'cc', by = fyear) + 
                 s(rain) + s(rollmean_rain) + visibility + s(veg_index), data = ema,
               family = negbin(theta_est_2), select = TRUE)

# Now do the validation tests for each model

# make functions to retrain models -------
retrain_ma = function(testyear){
  ma <<- filter(total, fyear < testyear) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
           fyear, season, pollen_cat, visibility, Dew.Point) %>%
    mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
           #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
           rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
           rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
           rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
           rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
           rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1),
           rollmean_visibility = lag(rollmean(visibility, 7, na.pad = T, align = 'right'), 1),
           rollmean_dewpoint = lag(rollmean(Dew.Point, 7, na.pad = T, align = 'right'), 1))
  
  # retrain model
  ma_1 <<- gam(pollen_count ~ 
                 s(max_temp) + s(rollmean_maxtemp) + 
                 s(rollmean_pollen) + s(min_temp)  +
                 s(humid) + 
                 s(rain) + s(rollmean_rain) +
                 s(wind_speed) + s(rollmean_windspeed) +
                 s(veg_index) + 
                 s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
                 s(ds, bs = 'cc', by = fyear) +
                 s(Dew.Point) + 
                 s(visibility), data = ma,
               family = nb(), scale = -0.1, select = TRUE)
  
  
  (theta_est_2 <<- ma_1$family$getTheta(TRUE))
  
  # With theta est
  ma_2 <<- gam(pollen_count ~ 
                 s(max_temp) + s(rollmean_maxtemp) + 
                 s(rollmean_pollen) + s(min_temp) +
                 s(humid) + 
                 s(rain) + s(rollmean_rain) +
                 s(wind_speed) + s(rollmean_windspeed) +
                 veg_index + 
                 s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
                 s(ds, bs = 'cc', by = fyear) +
                 s(Dew.Point) + 
                 s(visibility), data = ma,
               family = negbin(theta_est_2), select = TRUE)
}
retrain_ema = function(testyear){
  ema <<- filter(total, fyear < testyear) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
           wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
           Dew.Point) %>% 
    mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
           #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
           rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
           rollmean_rain      = lag(EMA(rain, 7), 1),
           rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
           rollmean_humid =  lag(EMA(humid, 7), 1),
           rollmean_winddir = lag(EMA(wind_dir, 7), 1),
           rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 
  
  # retrain model
  # Exponential moving average model
  
  ema_1 <<- gam(pollen_count ~ 
                  s(max_temp) + s(rollmean_maxtemp) + 
                  s(rollmean_pollen) + s(min_temp) +
                  s(humid) + 
                  s(rain, k = 20) + s(rollmean_rain) +
                  s(wind_speed) + s(rollmean_windspeed) +
                  veg_index + 
                  s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
                  s(ds, bs = 'cc', by = fyear) +
                  s(Dew.Point) + 
                  visibility, data = ema,
                family = nb(), scale = -0.1, select= TRUE)
  
  (theta_est <<- ema_1$family$getTheta(TRUE))
  
  # With theta est
  ema_2 <<- gam(pollen_count ~ 
                  s(max_temp) + s(rollmean_maxtemp) + 
                  s(rollmean_pollen) + s(min_temp) +
                  s(humid) + 
                  s(rain, k = 20) + s(rollmean_rain) +
                  s(wind_speed) + s(rollmean_windspeed) +
                  s(veg_index) + 
                  s(wind_dir, bs = 'cc') + s(rollmean_winddir, bs = 'cc') +
                  s(ds, bs = 'cc', by = fyear) +
                  s(Dew.Point) + 
                  visibility, data = ema,
                family = negbin(theta_est), select = TRUE)
}
retrain_simple = function(testyear){
  ema <<- filter(total, fyear < testyear) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
           wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
           Dew.Point) %>% 
    mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
           #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
           rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
           rollmean_rain      = lag(EMA(rain, 7), 1),
           rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
           rollmean_humid =  lag(EMA(humid, 7), 1),
           rollmean_winddir = lag(EMA(wind_dir, 7), 1),
           rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 
  
  # retrain model
  simple <<- gam(pollen_count ~ 
                   s(max_temp) +
                   rollmean_pollen +
                   s(ds, bs = 'cc') + 
                   s(rain) + s(rollmean_rain) + s(veg_index), data = ema,
                 family = nb(), scale = -0.1, select = TRUE)
  
  (theta_est_3 <<- simple$family$getTheta(TRUE))
  
  # With theta est
  simple_2 <<- gam(pollen_count ~ 
                     s(max_temp) +
                     rollmean_pollen +
                     s(ds, bs = 'cc') + 
                     s(rain) + s(rollmean_rain) + s(veg_index), data = ema,
                   family = negbin(theta_est_3), select = TRUE)
}

# Function to convert counts into categories
count_to_cat = function(counts){
  ds_cat = case_when(
    counts < 1 ~ "Very low",
    counts >= 1 & counts < 3 ~ "Low",
    counts >= 3 & counts < 8 ~ "Moderate",
    counts >= 8 & counts < 14.8 ~ "High",
    counts >= 14.8 ~ "Very high") %>%
    ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))
  return(ds_cat)
}

# function to return accuracy, MSE and MAE given a year and season
main_metrics = function(model, testyear, test_season){
  df = filter(total, season == test_season)
  if(model == 'ema'){
    retrain_ema(testyear) # retrain the model
    # now get an appropriate test set
    test = filter(df, fyear == testyear) %>% 
      select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
             wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
             Dew.Point) %>% 
      mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
             rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
             rollmean_rain      = lag(EMA(rain, 7), 1),
             rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
             rollmean_humid =  lag(EMA(humid, 7), 1),
             rollmean_winddir = lag(EMA(wind_dir, 7), 1),
             rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 
    # make predictions
    predicted_counts = exp(predict(ema_2, newdata = test))
    
    # convert these to categories
    predicted_cats = count_to_cat(predicted_counts)
    # not the real categories
    real_cats = test$pollen_cat
    
    # get a confusion matrix
    conf_matrix = caret::confusionMatrix(predicted_cats, real_cats)
    Accuracy = conf_matrix$overall[1]
    MSE = mse(conf_matrix$table)
    MAE = mae(conf_matrix$table)
    return(list(Accuracy = Accuracy,
                MSE = MSE,
                MAE = MAE))
  } else {
    if(model == 'ma'){
      retrain_ma(testyear) # retrain the model
      # now get an appropriate test set
      test = filter(df, fyear == testyear) %>% 
        select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
               fyear, season, pollen_cat, visibility, Dew.Point) %>%
        mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
               #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
               rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
               rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
               rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
               rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
               rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1),
               rollmean_visibility = lag(rollmean(visibility, 7, na.pad = T, align = 'right'), 1),
               rollmean_dewpoint = lag(rollmean(Dew.Point, 7, na.pad = T, align = 'right'), 1))
      # make predictions
      predicted_counts = exp(predict(ma_2, newdata = test))
      
      # convert these to categories
      predicted_cats = count_to_cat(predicted_counts)
      # not the real categories
      real_cats = test$pollen_cat
      
      # get a confusion matrix
      conf_matrix = caret::confusionMatrix(predicted_cats, real_cats)
      Accuracy = conf_matrix$overall[1]
      MSE = mse(conf_matrix$table)
      MAE = mae(conf_matrix$table)
      return(list(Accuracy = Accuracy,
                  MSE = MSE,
                  MAE = MAE))
    } else {
      retrain_simple(testyear) # retrain the model
      # now get an appropriate test set
      test = filter(df, fyear == testyear) %>% 
        select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
               wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
               Dew.Point) %>% 
        mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
               rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
               rollmean_rain      = lag(EMA(rain, 7), 1),
               rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
               rollmean_humid =  lag(EMA(humid, 7), 1),
               rollmean_winddir = lag(EMA(wind_dir, 7), 1),
               rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 
      # make predictions
      predicted_counts = exp(predict(simple, newdata = test))
      
      # convert these to categories
      predicted_cats = count_to_cat(predicted_counts)
      # not the real categories
      real_cats = test$pollen_cat
      
      # get a confusion matrix
      conf_matrix = caret::confusionMatrix(predicted_cats, real_cats)
      Accuracy = conf_matrix$overall[1]
      MSE = mse(conf_matrix$table)
      MAE = mae(conf_matrix$table)
      return(list(Accuracy = Accuracy,
                  MSE = MSE,
                  MAE = MAE))
    }
  }
} 

# Function to return total accuracy, MSE and MAE for a given year
main_metrics_total = function(model, testyear){
  df = total
  if(model == 'ema'){
    retrain_ema(testyear) # retrain the model
    # now get an appropriate test set
    test = filter(df, fyear == testyear) %>% 
      select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
             wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
             Dew.Point) %>% 
      mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
             rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
             rollmean_rain      = lag(EMA(rain, 7), 1),
             rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
             rollmean_humid =  lag(EMA(humid, 7), 1),
             rollmean_winddir = lag(EMA(wind_dir, 7), 1),
             rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 
    # make predictions
    predicted_counts = exp(predict(ema_2, newdata = test))
    
    # convert these to categories
    predicted_cats = count_to_cat(predicted_counts)
    # not the real categories
    real_cats = test$pollen_cat
    
    # get a confusion matrix
    conf_matrix = caret::confusionMatrix(predicted_cats, real_cats)
    Accuracy = conf_matrix$overall[1]
    MSE = mse(conf_matrix$table)
    MAE = mae(conf_matrix$table)
    return(list(Accuracy = Accuracy,
                MSE = MSE,
                MAE = MAE))
  } else {
    if(model == 'ma'){
      retrain_ma(testyear) # retrain the model
      # now get an appropriate test set
      test = filter(df, fyear == testyear) %>% 
        select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
               fyear, season, pollen_cat, visibility, Dew.Point) %>%
        mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
               rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
               rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
               rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
               rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
               rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1),
               rollmean_visibility = lag(rollmean(visibility, 7, na.pad = T, align = 'right'), 1),
               rollmean_dewpoint = lag(rollmean(Dew.Point, 7, na.pad = T, align = 'right'), 1))
      # make predictions
      predicted_counts = exp(predict(ma_2, newdata = test))
      
      # convert these to categories
      predicted_cats = count_to_cat(predicted_counts)
      # not the real categories
      real_cats = test$pollen_cat
      
      # get a confusion matrix
      conf_matrix = caret::confusionMatrix(predicted_cats, real_cats)
      Accuracy = conf_matrix$overall[1]
      MSE = mse(conf_matrix$table)
      MAE = mae(conf_matrix$table)
      return(list(Accuracy = Accuracy,
                  MSE = MSE,
                  MAE = MAE))
    } else {
      retrain_simple(testyear) # retrain the model
      # now get an appropriate test set
      test = filter(df, fyear == testyear) %>% 
        select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
               wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
               Dew.Point) %>% 
        mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
               rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
               rollmean_rain      = lag(EMA(rain, 7), 1),
               rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
               rollmean_humid =  lag(EMA(humid, 7), 1),
               rollmean_winddir = lag(EMA(wind_dir, 7), 1),
               rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) 
      # make predictions
      predicted_counts = exp(predict(simple, newdata = test))
      
      # convert these to categories
      predicted_cats = count_to_cat(predicted_counts)
      # not the real categories
      real_cats = test$pollen_cat
      
      # get a confusion matrix
      conf_matrix = caret::confusionMatrix(predicted_cats, real_cats)
      Accuracy = conf_matrix$overall[1]
      MSE = mse(conf_matrix$table)
      MAE = mae(conf_matrix$table)
      return(list(Accuracy = Accuracy,
                  MSE = MSE,
                  MAE = MAE))
    }
  }
} 

# function returns probabilities 
prop = function(r){
  vl = length(which(r=="Very low"))
  l = length(which(r=="Low"))
  m = length(which(r=="Moderate"))
  h = length(which(r=="High"))
  vh = length(which(r=="Very high"))
  
  return(c(vl, l, m, h, vh)/nsims)
}

# Brier score testing -------
brier_results = function(year, test_season, nsims = 1000){
  nsims <<- nsims
  df = filter(total, season == test_season)
  
  # retrain all models
  retrain_ma(year)
  retrain_ema(year)
  retrain_simple(year)
  
  # EMA test set
  ema_test <<- filter(df, fyear == year) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
           wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
           Dew.Point) %>% 
    mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
           rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
           rollmean_rain      = lag(EMA(rain, 7), 1),
           rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
           rollmean_humid =  lag(EMA(humid, 7), 1),
           rollmean_winddir = lag(EMA(wind_dir, 7), 1),
           rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) %>% na.omit()
  
  # MA test set
  ma_test <<- filter(df, fyear == year) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
           fyear, season, pollen_cat, visibility, Dew.Point) %>%
    mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
           rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
           rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
           rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
           rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
           rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1),
           rollmean_visibility = lag(rollmean(visibility, 7, na.pad = T, align = 'right'), 1),
           rollmean_dewpoint = lag(rollmean(Dew.Point, 7, na.pad = T, align = 'right'), 1)) %>%
    na.omit()
  
  # make predictions
  ema_predict = exp(predict(ema_2, newdata = ema_test))
  
  ma_predict = exp(predict(ma_2, newdata = ma_test))
  
  simple_predict = exp(predict(simple_2, newdata = ema_test))
  
  # Empty matrices for samples
  ema_samples = matrix(ncol = nsims, nrow = length(ema_predict))
  ma_samples = matrix(ncol = nsims, nrow = length(ma_predict))
  simple_samples = matrix(ncol = nsims, nrow = length(simple_predict))
  
  # Make samples from neg binom
  for(i in 1:length(ma_predict)){
    ma_samples[i,] = MASS::rnegbin(nsims, mu = ma_predict[i], theta = theta_est_2)
  }
  
  for(i in 1:length(ema_predict)){
    ema_samples[i,] = MASS::rnegbin(nsims, mu = ema_predict[i], theta = theta_est)
  }
  
  for(i in 1:length(simple_predict)){
    simple_samples[i,] = MASS::rnegbin(nsims, mu = simple_predict[i], theta = theta_est_3)
  }
  
  # convert samples to categories
  ma_samples = apply(ma_samples, 2, count_to_cat)
  ema_samples = apply(ema_samples, 2, count_to_cat)
  simple_samples = apply(simple_samples, 2, count_to_cat)
  
  # matrices for probabilities
  ema_probs = matrix(ncol = 5, nrow = nrow(ema_samples))
  ma_probs = matrix(ncol = 5, nrow = nrow(ma_samples))
  simple_probs = matrix(ncol = 5, nrow = nrow(simple_samples))
  
  # Populate probability matrices
  for(i in 1:nrow(ema_samples)){
    ema_probs[i, ] = prop(ema_samples[i,])
  }
  
  for(i in 1:nrow(ma_samples)){
    ma_probs[i, ] = prop(ma_samples[i,])
  } 
  
  for(i in 1:nrow(simple_samples)){
    simple_probs[i, ] = prop(simple_samples[i,])
  } 
  
  return(list(ema_probs = ema_probs, ma_probs = ma_probs,
              simple_probs = simple_probs))
}

# second function for brier results
final_brier = function(results, validation){
  validation = na.omit(validation)
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
  brierData[, 1:5] = results
  return(brier_score(brierData))
}

brier_results_total = function(year, nsims = 1000){
  nsims <<- nsims
  df = total
  
  # retrain all models
  retrain_ma(year)
  retrain_ema(year)
  retrain_simple(year)
  
  # EMA test set
  ema_test <<- filter(df, fyear == year) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, 
           wind_speed, wind_dir, fyear, season, pollen_cat, visibility,
           Dew.Point) %>% 
    mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
           rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
           rollmean_rain      = lag(EMA(rain, 7), 1),
           rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
           rollmean_humid =  lag(EMA(humid, 7), 1),
           rollmean_winddir = lag(EMA(wind_dir, 7), 1),
           rollmean_dewpoint = lag(EMA(Dew.Point, 7), 1)) %>% na.omit()
  
  # MA test set
  ma_test <<- filter(df, fyear == year) %>% 
    select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
           fyear, season, pollen_cat, visibility, Dew.Point) %>%
    mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
           rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
           rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
           rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
           rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
           rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1),
           rollmean_visibility = lag(rollmean(visibility, 7, na.pad = T, align = 'right'), 1),
           rollmean_dewpoint = lag(rollmean(Dew.Point, 7, na.pad = T, align = 'right'), 1)) %>%
    na.omit()
  
  # make predictions
  ema_predict = exp(predict(ema_2, newdata = ema_test))
  
  ma_predict = exp(predict(ma_2, newdata = ma_test))
  
  simple_predict = exp(predict(simple_2, newdata = ema_test))
  
  # Empty matrices for samples
  ema_samples = matrix(ncol = nsims, nrow = length(ema_predict))
  ma_samples = matrix(ncol = nsims, nrow = length(ma_predict))
  simple_samples = matrix(ncol = nsims, nrow = length(simple_predict))
  
  # Make samples from neg binom
  for(i in 1:length(ma_predict)){
    ma_samples[i,] = MASS::rnegbin(nsims, mu = ma_predict[i], theta = theta_est_2)
  }
  
  for(i in 1:length(ema_predict)){
    ema_samples[i,] = MASS::rnegbin(nsims, mu = ema_predict[i], theta = theta_est)
  }
  
  for(i in 1:length(simple_predict)){
    simple_samples[i,] = MASS::rnegbin(nsims, mu = simple_predict[i], theta = theta_est_3)
  }
  
  # convert samples to categories
  ma_samples = apply(ma_samples, 2, count_to_cat)
  ema_samples = apply(ema_samples, 2, count_to_cat)
  simple_samples = apply(simple_samples, 2, count_to_cat)
  
  # matrices for probabilities
  ema_probs = matrix(ncol = 5, nrow = nrow(ema_samples))
  ma_probs = matrix(ncol = 5, nrow = nrow(ma_samples))
  simple_probs = matrix(ncol = 5, nrow = nrow(simple_samples))
  
  # Populate probability matrices
  for(i in 1:nrow(ema_samples)){
    ema_probs[i, ] = prop(ema_samples[i,])
  }
  
  for(i in 1:nrow(ma_samples)){
    ma_probs[i, ] = prop(ma_samples[i,])
  } 
  
  for(i in 1:nrow(simple_samples)){
    simple_probs[i, ] = prop(simple_samples[i,])
  } 
  
  return(list(ema_probs = ema_probs, ma_probs = ma_probs,
              simple_probs = simple_probs))
}
