
###############

In 00_weather_data.R all weather files from 2000-2020 are loaded and the data is merged.
Also in this file: imputation of missing data and covariate creation from 2002-2020 since we do not have population data before 2002.

In 00_point_to_area.R we identify the districts of the states.

Then change to 00_kNN.R to get meteorological data for districts without monitoring stations. Afterwards change back to 00_point_to_area.R and aggregate the point meteorological data to district data.

In 00_population_mortality_district_data.R all population files (mortality data) from 2002-2020 are loaded and pre-processed.

In 00_inla_mortality_districts.R the model is constructed and inferred.
