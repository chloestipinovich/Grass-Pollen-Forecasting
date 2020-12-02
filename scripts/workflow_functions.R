# Title       : workflow
# Last Edited : November 2020
# Authors     : Sky Cope and Chloë Stipinovich


# Function APIrequest
# Gets API weather data 
# past 7 days weather data & 7 days ahead forecasts
APIrequest = function(){
  keys     = c("D93BEE3MG1DDV9HFKD4TDZ2B3","PZSMCLY0TV8WDVR85EKPBLMYQ", "94PTQDZ21VSDMHHT2XVFJK40U", "EBAR2HSG4KACLUX2B3E8XEHXU",
               "YGRJRH8DV62KZ7MAW43SQL900", "EAPTA3QB308L41LWVE35DYEYL", "HA5E9345NW0XENSZCQC5RY4WT")
  # Forecast
  request_future     = GET(paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/forecast?location=-33.9345,18.4771&aggregateHours=24&unitGroup=metric&shortColumnNames=false&contentType=json&key=", keys[1], sep = ""))
  (forecast          = fromJSON(rawToChar(request_future$content))$locations$'-33.9345,18.4771'$values)
  forecast$datetimeStr = as.Date(forecast$datetimeStr)
  forecast = forecast[,c("datetimeStr", "mint", "maxt", "humidity", "precip","wspd", "wdir")]
  forecast = forecast %>% rename(date = datetimeStr, min_temp = mint, max_temp = maxt, humid = humidity, rain = precip, wind_speed = wspd, wind_dir = wdir)
  # Historical
  startDate         = Sys.Date()-7
  endDate           = Sys.Date() 
  request_history   = read.csv(paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?&aggregateHours=24&startDateTime=", startDate, "&endDateTime=", endDate, 
                                     "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=-33.9345,18.4771&key=", keys[1], sep = ""))
  request_history$Date.time = as.Date(request_history$Date.time, format = "%m/%d/%Y")
  historical = request_history[,c("Date.time", "Minimum.Temperature", "Maximum.Temperature", "Relative.Humidity", "Precipitation", "Wind.Speed", "Wind.Direction")]
  historical = historical %>% rename(date = Date.time, min_temp = Minimum.Temperature, max_temp = Maximum.Temperature, humid = Relative.Humidity, rain = Precipitation, wind_speed = Wind.Speed, wind_dir = Wind.Direction)
  # Combined
  return(as.data.frame(rbind(historical[2:8,], forecast[2:8,])))
}

# Functionfetch_pollen
# Fetches pollen data from github repository from past 7 days
fetch_pollen = function(){
  pollen = read.csv('https://raw.githubusercontent.com/skycope/grass-pollen/master/Workflow/pollen_counts.csv')
  pollen = data.frame(pollen_count = c(pollen$pollen_count, rep(NA, 7)))
  return(pollen)
}

# Function fetch_vegindex
# Fetch most recent Veg Index data 
fetch_vegindex = function(){
  vegetation = read.csv('https://raw.githubusercontent.com/skycope/grass-pollen/master/Workflow/veg_index_new.csv')
  vegetation = data.frame(veg_index = rep(vegetation$veg_index, 14))
  return(vegetation)
}

# Function get_dates
# Get dates for correct 2 week period
get_dates = function(){
  dates = seq(Sys.Date() - 6, Sys.Date() + 7, by = 1)
  return(dates)
}

# Function get_season
# Gets the season (In Season or Out of Season) given days since
get_season = function(ds){
  season = case_when(
    ds > 240 | ds < 30 ~ "In Season",
    ds <= 240 | ds >= 30 ~ "Not in Season"
  )
  return(season)
}

# Function get_year
# Gets correct year given dates
get_year = function(dates){
  return(lubridate::year(dates))
}

# Function lags
# Creates required moving averages for prediction:
# - dat    = two week data set
# - rng    = 8 day period, 8th day is the prediction day
# - output = single row of data with all variables required to make prediction
lags = function(dat, rng){
  output = dat[rng,] %>%
    mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
           rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
           rollmean_rain      = lag(EMA(rain, 7), 1),
           rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
           rollmean_humid     = lag(EMA(humid, 7), 1),
           rollmean_winddir   = lag(EMA(wind_dir, 7), 1),
           lag2_rain          = lag(rain, 1),
           lag2_pollen        = lag(pollen_count, 1))
  return(output[8,])
}

# Function GAM_predict
# performs prediction using ema_2
# - model = best GAM model with corresponding theta_est
# - day   = single row of all variables needed for prediction including moving averages
GAM_predict = function(model, day){
  return(exp(predict(model, day)))
}

# Function freq
# returns Table of frequencies
# - pred = point estimate from day ahead prediction
# - n    = number of samples from neg binomial
# - table(dist_cat)/n = table of category probabilities
freq = function(pred, n){
  dist     = MASS::rnegbin(n, mu = pred, theta = theta_est)
  dist_cat = case_when(
    dist < 1 ~ "Very Low",
    dist >= 1 & dist < 3 ~ "Low",
    dist >= 3 & dist < 8 ~ "Moderate",
    dist >= 8 & dist < 14.8 ~ "High",
    dist >= 14.8 ~ "Very High") %>%
    ordered(., levels = c("Very Low", "Low", "Moderate", "High", "Very High"))
  return(list(freq_table = table(dist_cat)/n, samples = dist))
}

# Function freq2
# returns Table of frequencies from n_sample samples
# - dist = row of samples
# - n    = number of samples 
# - table(dist_cat)/n = table of category probabilities
freq2 = function(dist, n){
  dist_cat = case_when(
    dist < 1 ~ "Very Low",
    dist >= 1 & dist < 3 ~ "Low",
    dist >= 3 & dist < 8 ~ "Moderate",
    dist >= 8 & dist < 14.8 ~ "High",
    dist >= 14.8 ~ "Very High") %>%
    ordered(., levels = c("Very Low", "Low", "Moderate", "High", "Very High"))
  return(table(dist_cat)/n)
}

# Function past 
# returns the correct historic data for a specific sample path
# - sample_row = sample path out of 1 to n_samples
# - num days   = number of days forward the sample path has predicted so far 
# - past_sample_data = two week period with specific sample path data
past = function(sample_row, num_days){
  past_sample_data = twoWeeks
  for (day in 1:num_days){
    past_sample_data$pollen_count[day+7]  = past_samples[day,sample_row]
  }
  return(past_sample_data)
}
