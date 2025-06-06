library(dplyr)
library(tidyverse)
library(VIM)
library(colorspace)
library(grid)

load("01_next_step_kNN.R")


pMiss<-function(x){sum(is.na(x))/length(x)*100}
apply(weather_data_all_dry_weeks_districts, 2, pMiss)

test<-weather_data_all_dry_weeks_districts[!complete.cases(weather_data_all_dry_weeks_districts),]
districts_with_na<-unique(test$District)

nearest_neighbours <- function(df, districts_with_na, k) {
  
  
  cols_for_kNN<-c("Temp_mean","Temp_min_mean", "Temp_max_mean", "Temp_min", "Temp_max", "Precip_mean",
                  "Humidity_mean", #"Pressure_mean",
                  "Temp_spread_day", "Length_no_rain", "Last_week_was_dry", "Cold_week", "Hot_week",
                  "Mild_week", "Tropical_week", "Super_cold_week", "Strong_discomfort_humidity", "Severe_malaise_humidity",
                  "Increased_risk_humidity", "Serious_risk_humidity")
  
  
  for(d in 1:length(districts_with_na)){
    
    cat("\nWe have", d, "from", length(districts_with_na))
    
    district_data<- df %>% filter(District==districts_with_na[d])
    
    year_week<-unique(district_data %>% select(Year, Week))
    
    for(i in 1:nrow(year_week)){
      
      year<-year_week$Year[i]
      week<-year_week$Week[i]
      
      weekly_data<-df %>% filter( Year==year, Week %in% c( week))
      
      imputed_weekly_data<-kNN(weekly_data, variable=cols_for_kNN,
                               k=k,
                               dist_var=c("Longitude", "Latitude", "Year", "Week"),
                               imp_var=F)      #set imp_var to F to avoid addition
      
      df<-df %>% rows_update(imputed_weekly_data, by=c("Longitude", "Latitude", "Year", "Week"))
      
      
    }
    
  }
  
  return(df)
  
}


weather_data_all_dry_weeks_districts_kNN<-nearest_neighbours(weather_data_all_dry_weeks_districts, districts_with_na,k=3)
save(weather_data_all_dry_weeks_districts_kNN,Austrian_districts_combined, file="01_kNN_done.R")
