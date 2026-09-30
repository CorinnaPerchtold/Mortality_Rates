
###############

In 00_weather_data.R all weather files from 2000-2019 are loaded and the data is merged.
Output: 01_raw_data.R

Next open 00_projected_weather_data_daily.R. We downscale precipitation, temp mean/min/max and humidity mean on a grid and aggregate it to daily district-level data.
Output: 01_district_daily_downscaled_finer_grid.RData

In 00_district_weekly_covariates.R weekly covariates are defined based on daily district-level criteria.
Output: 01_district_weekly_covariates_finer_grid.RData

The file 00_population_mortality_district.R gathers the population data in 01_population_districts.R and then merges with the mortality counts data, which we do not upload. 

In 01_Austrian_districts.RData you find the object of the Austrian districts with their respective mean elevation. 

In 00_model_comparison.R we evaluate the best of 14 mortality models for both genders jointly, where the complexity of the models is steadily increasing.

