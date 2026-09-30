# In this file:  build weekly district-level covariates from the daily downscaled district data


library(dplyr)
library(tidyverse)
library(lubridate)
library(omnibus)   # longRun()
library(magrittr)

load("01_district_daily_downscaled_finer_grid.RData") 


# daily series should have ISO week/year keys 
district_daily$Day<-day(district_daily$Date)
district_daily$Week<-isoweek(district_daily$Date)
district_daily$Month<-month(district_daily$Date)
district_daily$Year<-isoyear(district_daily$Date)


sd_cols <- grep("_sd$", names(district_daily), value = TRUE)

#function to summarise daily to weekly district-level data based on the daily 
#criteria that define e.g. hot week
summarise_week <- function(wk, sd_cols) {
  
  wk <- wk[order(wk$Date), ]
  wk$Temp_spread_day <- wk$Temp_max - wk$Temp_min
  wk$Length_no_rain  <- longRun(wk$Precip_sum, 0)
  
  cons_dry <- cold <- super_cold <- hot <- mild <- tropical <- 0L
  f_dry <- f_cold <- f_super <- f_hot <- f_mild <- f_trop <- FALSE
  f_strong <- f_severe <- f_incr <- f_serious <- FALSE
  
  for (j in seq_len(nrow(wk))) {
    Tmin <- wk$Temp_min[j]
    Tmax <- wk$Temp_max[j]
    Tmea <- wk$Temp_mean[j]
    Hum <- wk$Humidity_mean[j]
    Pre  <- wk$Precip_sum[j]
    
    if (!is.na(Pre) && Pre == 0 &&
        ((!is.na(Tmea) && Tmea > 24) || (!is.na(Tmea) && Tmea < -1))) {
      cons_dry <- cons_dry + 1L
      if (cons_dry >= 3) f_dry <- TRUE
    } else cons_dry <- 0L
    
    if (!is.na(Tmin) && Tmin < 0) 
      { cold <- cold + 1L; if (cold >= 3) f_cold <- TRUE } else cold <- 0L
    if (!is.na(Tmin) && Tmin < -5) 
      { super_cold <- super_cold + 1L; if (super_cold >= 2) f_super <- TRUE } else super_cold <- 0L
    if (!is.na(Tmin) && Tmin > 18) 
      { hot <- hot + 1L; if (hot >= 3) f_hot <- TRUE } else hot <- 0L
    if (!is.na(Tmea) && Tmea > 2 && Tmea < 9) 
      { mild <- mild + 1L; if (mild >= 3) f_mild <- TRUE } else mild <- 0L
    if (!is.na(Tmax) && Tmax > 29) 
      { tropical <- tropical + 1L; if (tropical >= 2) f_trop <- TRUE } else tropical <- 0L
    
    if (!is.na(Tmax) && !is.na(Hum)) {
      strong <- (
        (Tmax>33 & Tmax<37 & Hum<30) |
          (Tmax>32 & Tmax<36 & Hum>29 & Hum<35) |
          (Tmax>31 & Tmax<35 & Hum>34 & Hum<40) |
          (Tmax>30 & Tmax<34 & Hum>39 & Hum<45) |
          (Tmax>29 & Tmax<33 & Hum>44 & Hum<50) |
          (Tmax>28 & Tmax<32 & Hum>49 & Hum<55) |
          (Tmax>28 & Tmax<31 & Hum>54 & Hum<60) |
          (Tmax>27 & Tmax<31 & Hum>59 & Hum<65) |
          (Tmax>27 & Tmax<30 & Hum>64 & Hum<70) |
          (Tmax>26 & Tmax<30 & Hum>69 & Hum<75) |
          (Tmax>26 & Tmax<29 & Hum>74 & Hum<80) |
          (Tmax>25 & Tmax<29 & Hum>79 & Hum<85) |
          (Tmax>25 & Tmax<28 & Hum>84 & Hum<90) |
          (Tmax>24 & Tmax<28 & Hum>89 & Hum<95) |
          (Tmax>23 & Tmax<27 & Hum>94))
      if (isTRUE(strong)) f_strong <- TRUE
      
      severe <- (
        (Tmax>36 & Tmax<41 & Hum<30) |
          (Tmax>35 & Tmax<40 & Hum>29 & Hum<35) |
          (Tmax>34 & Tmax<39 & Hum>34 & Hum<40) |
          (Tmax>33 & Tmax<38 & Hum>39 & Hum<45) |
          (Tmax>32 & Tmax<37 & Hum>44 & Hum<50) |
          (Tmax>31 & Tmax<36 & Hum>49 & Hum<55) |
          (Tmax>30 & Tmax<35 & Hum>54 & Hum<60) |
          (Tmax>30 & Tmax<34 & Hum>59 & Hum<65) |
          (Tmax>29 & Tmax<33 & Hum>64 & Hum<75) |
          (Tmax>28 & Tmax<32 & Hum>74 & Hum<85) |
          (Tmax>27 & Tmax<31 & Hum>84 & Hum<90) |
          (Tmax>27 & Tmax<30 & Hum>89 & Hum<95) |
          (Tmax>26 & Tmax<29 & Hum>94))
      if (isTRUE(severe)) f_severe <- TRUE
      
      incr <- (
        (Tmax>40 & Tmax<43 & Hum<30) |
          (Tmax>39 & Tmax<43 & Hum>29 & Hum<35) |
          (Tmax>38 & Tmax<43 & Hum>34 & Hum<40) |
          (Tmax>37 & Tmax<42 & Hum>39 & Hum<45) |
          (Tmax>36 & Tmax<41 & Hum>44 & Hum<50) |
          (Tmax>35 & Tmax<40 & Hum>49 & Hum<55) |
          (Tmax>34 & Tmax<39 & Hum>54 & Hum<60) |
          (Tmax>33 & Tmax<38 & Hum>59 & Hum<65) |
          (Tmax>32 & Tmax<37 & Hum>64 & Hum<70) |
          (Tmax>32 & Tmax<36 & Hum>69 & Hum<75) |
          (Tmax>31 & Tmax<36 & Hum>74 & Hum<80) |
          (Tmax>31 & Tmax<35 & Hum>79 & Hum<85) |
          (Tmax>30 & Tmax<34 & Hum>84 & Hum<90) |
          (Tmax>29 & Tmax<33 & Hum>89 & Hum<95))
      if (isTRUE(incr)) f_incr <- TRUE
      
      serious <- (
        (Tmax>39 & Tmax<43 & Hum>39 & Hum<55) |
          (Tmax>38 & Tmax<43 & Hum>54 & Hum<60) |
          (Tmax>37 & Tmax<43 & Hum>59 & Hum<65) |
          (Tmax>36 & Tmax<43 & Hum>64 & Hum<70) |
          (Tmax>35 & Tmax<43 & Hum>69 & Hum<80) |
          (Tmax>34 & Tmax<43 & Hum>79 & Hum<85) |
          (Tmax>33 & Tmax<43 & Hum>84 & Hum<95) |
          (Tmax>32 & Tmax<43 & Hum>94))
      if (isTRUE(serious)) f_serious <- TRUE
    }
  }
  
  wk_sd <- if (length(sd_cols)) as.list(colMeans(wk[sd_cols], na.rm = TRUE)) else list()
  
  res <- tibble(
    Date            = min(wk$Date),
    n_days          = nrow(wk),
    Temp_mean       = round(mean(wk$Temp_mean), 3),
    Temp_min_mean   = round(mean(wk$Temp_min), 3),
    Temp_max_mean   = round(mean(wk$Temp_max), 3),
    Temp_min        = round(min(wk$Temp_min), 3),
    Temp_max        = round(max(wk$Temp_max), 3),
    Precip_mean     = round(mean(wk$Precip_sum), 3),
    Humidity_mean   = round(mean(wk$Humidity_mean), 3),
    Temp_spread_day = round(mean(wk$Temp_spread_day), 3),
    Length_no_rain  = mean(wk$Length_no_rain),
    Cold_week                  = as.integer(f_cold),
    Super_cold_week            = as.integer(f_super),
    Hot_week                   = as.integer(f_hot),
    Mild_week                  = as.integer(f_mild),
    Tropical_week              = as.integer(f_trop),
    Dry_week                   = as.integer(f_dry),
    Strong_discomfort_humidity = as.integer(f_strong),
    Severe_malaise_humidity    = as.integer(f_severe),
    Increased_risk_humidity    = as.integer(f_incr),
    Serious_risk_humidity      = as.integer(f_serious)
  )
  if (length(wk_sd)) res <- bind_cols(res, as_tibble(wk_sd))
  res
}


district_weekly <- district_daily %>%
  group_by(District, Year, Week) %>%
  group_modify(~ summarise_week(.x, sd_cols)) %>%
  ungroup()

save(district_weekly, 
     file = "01_district_weekly_covariates_finer_grid.RData")

