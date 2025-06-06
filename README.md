
###############

#1. in 00_weather_data.R all weather files from 2000-2020 are loaded and the data is merged
#   also in this file: imputation of missing data and covariate creation from 2002-2020 since we do not have population data before 2002

#2. in 00_point_to_area.R we identify the districts of the states

#3. then change to 00_kNN.R to get meteorological data for districts without monitoring stations,
#   afterwards change back to 00_point_to_area.R and aggregate the point meteorological data to district data

#4. 00_population_mortality_district_data.R all population files from 2002-2020 are loaded and pre-processed
#   the same for the paid mortality data from 2000-2020
#   then we merge the data from 2002-2020

#5. 00_inla_mortality_districts.R does the inference
