# Title       : workflow
# Last Edited : November 2020
# Authors     : Sky Cope and Chloë Stipinovich

# Step 1: Install required packages
{
  library(request)
  library(jsonlite)
  library(httr)
  library(zoo)
  library(pastecs)
  library(data.table)
  library(corrplot)
  library(dplyr)
  library(tidyverse)
  library(reshape2)
  library(mgcv)
  library(MASS)
  library(dplyr)
}

# Step 2: Set working directory
setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/Final Code/Grass-Pollen-Forecasting/scripts")

# Step 3: load GAM model and functions from workflow_functions.R script
gamModel = readRDS('../data/gamModel.rds')
source('workflow_functions.R')
# Set a two week period ----------
apiData       = APIrequest()
pollenData    = fetch_pollen()
vegIndexData  = fetch_vegindex()
theDates      = get_dates()
ds            = lubridate::yday(theDates)
season        = get_season(dsData)
fyear         = get_year(theDates)

twoWeeks      = cbind(pollenData, ds, apiData, vegIndexData, fyear, season)
twoWeeks   = twoWeeks %>%  
  dplyr::select(pollen_count, ds, min_temp, max_temp,
                veg_index, humid, rain, wind_speed, wind_dir, 
                fyear, season)

# Initiate Storage for Predictions
predictions        = as.data.frame(matrix(NA, nrow = 7, ncol = 5))
names(predictions) = c("Very_Low", "Low", "Moderate", "High", "Very_High")

# Make 7-day-ahead predictions 
# Update twoWeeks data set as you make a new prediction
for (i in 1:7){
  day_ahead       = lags(twoWeeks, c(i:(i+7) ) )
  pred            = as.numeric(GAM_predict(ema_2, day_ahead))
  results         = freq(pred, n_samples)
  predictions[i,] = results$freq_table
  past_samples[i,]= results$samples   # Save random samples called posterior samples
  twoWeeks$pollen_count[i+7] = pred
}


# Write predictions to csv
write.csv(predictions, '../data/predictions.csv')
