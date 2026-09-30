#In this file: model-comparison grid 

library(spdep)
library(spData)
library(INLA)
library(Matrix)
library(sp)
library(sf)
library(gtools)
library(ggplot2)
library(dplyr)
library(matrixcalc)
library(viridis)
library(viridisLite)
library(tidyr)
library(dlnm)

load("01_mortality_data_districts_all_years.R")
load("03_district_weekly_covariates_finer_grid.RData")
load("01_Austrian_districts.RData")

setwd("/gpfs/scratch/perchtold/Mortality")
source("kronecker_nullspace.R")

#build neighbourhood matrix
districts_sf <- read_sf("gadm41_AUT_2.shp")
Austria_districts <- poly2nb(districts_sf)
nb2INLA("Austria_districts.graph", Austria_districts)
inla.setOption(scale.model.default = FALSE)
H <- inla.read.graph(file = "Austria_districts.graph")

#merge data and order
data_district <- data_district[order(data_district$Year), ]
data_district <- data_district %>% arrange(Date) %>% mutate(Time = dense_rank(Date))
district_weekly <- district_weekly %>% arrange(Date) %>% mutate(Time = dense_rank(Date))
df_CAR <- merge(Austrian_districts_combined, data_district,
                by = c("District"))

df_CAR <- df_CAR[order(df_CAR$Date), ]

df_CAR <- merge(df_CAR, district_weekly, by = c("District", "Time", "Year", "Week", "Date"))
df_CAR <- df_CAR[order(df_CAR$Date), ]

#build dlnm model with 3 weeks lag
max_lag <- 3
df_unique <- df_CAR %>%
  sf::st_drop_geometry() %>%                      
  dplyr::select(District, Time, Temp_mean) %>%
  distinct() %>%
  arrange(District, Time)

cb_temp <- crossbasis(
  x = df_unique$Temp_mean, lag = c(0, max_lag),
  argvar = list(fun = "ns", df = 4,
                knots = quantile(df_unique$Temp_mean, c(0.10, 0.75, 0.90), na.rm = TRUE)),
  arglag = list(fun = "ns", df = 2),
  group = df_unique$District
)
cb_mat <- as.matrix(cb_temp)
cb_cols <- paste0("cb_v", seq_len(ncol(cb_mat)))
colnames(cb_mat) <- cb_cols
cb_df <- cbind(df_unique[, c("District", "Time")], as.data.frame(cb_mat))

df_joined <- df_CAR %>% left_join(cb_df, by = c("District", "Time"))

#first 3 weeks are NA now after building "lags", remove them
df_model <- df_joined %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(complete.cases(dplyr::across(dplyr::all_of(cb_cols))))

# re-rank Time to a clean contiguous 1..N.
df_model$Time <- as.integer(factor(df_model$Time, levels = sort(unique(df_model$Time))))
df_model$ID   <- as.integer(factor(df_model$District, levels = unique(df_model$District)))

# verify Time is a clean contiguous index
stopifnot(identical(sort(unique(df_model$Time)), 1:length(unique(df_model$Time))))

new_df <- data.frame(
  Deaths = df_model$Deaths, Gender = df_model$Gender, Age = df_model$Age, Date = df_model$Date,
  District = df_model$District, Population = df_model$Population,
  ID = df_model$ID, Time = df_model$Time, Year = df_model$Year, Week = df_model$Week,
  Temp_mean = df_model$Temp_mean,
  Humidity_mean = df_model$Humidity_mean,
  Super_cold_week = df_model$Super_cold_week,
  Cold_week = df_model$Cold_week,
  Hot_week = df_model$Hot_week,
  Tropical_week = df_model$Tropical_week,
  Mild_week = df_model$Mild_week,
  Increased_risk_humidity = df_model$Increased_risk_humidity,
  Serious_risk_humidity = df_model$Serious_risk_humidity,
  Strong_discomfort_humidity = df_model$Strong_discomfort_humidity,
  Severe_malaise_humidity = df_model$Severe_malaise_humidity,
  Elevation = df_model$Elevation
)
new_df <- cbind(new_df, df_model[, cb_cols])
stopifnot(sum(is.na(new_df[, cb_cols])) == 0)

districts  <- length(unique(df_model$District))
timepoints <- length(unique(df_model$Time))
ages       <- length(unique(df_model$Age))

new_df$Age <- factor(new_df$Age, levels = c("0-64", "65-74", "75-84", "85+"))

# build interaction indices for Type IV terms
new_df_male   <- new_df[new_df$Gender == "Male", ]
new_df_female <- new_df[new_df$Gender == "Female", ]
for (nm in c("male", "female")) {
  d <- get(paste0("new_df_", nm))
  d <- d[mixedorder(d$Age), ]; d <- d[order(d$District), ]; d <- d[order(d$Time), ]
  d$ID.prov.age  <- rep(seq(1, districts * ages), timepoints)
  d$ID.prov.week <- rep(seq(1, districts * timepoints), each = ages)
  d$ID.age.week  <- as.vector(apply(matrix(seq(1, ages * timepoints), ages, timepoints), 2,
                                    function(x) rep(x, districts)))
  assign(paste0("new_df_", nm), d)
}
new_df <- rbind(new_df_male, new_df_female)

#check dimensions
stopifnot(nrow(new_df_male)   == districts * ages * timepoints)
stopifnot(nrow(new_df_female) == districts * ages * timepoints)

#prepare for Gender deviation terms
new_df$Gender  <- factor(new_df$Gender, levels = c("Female", "Male"))
new_df$is_male <- ifelse(new_df$Gender == "Male", 1, 0)

# male-only deviation indices (NA for females -> females get baseline only)
new_df$ID_male   <- ifelse(new_df$Gender == "Male", new_df$ID, NA)              
new_df$Age_int   <- as.integer(new_df$Age)
new_df$Age_male  <- ifelse(new_df$Gender == "Male", new_df$Age_int, NA)         
new_df$Time_male <- ifelse(new_df$Gender == "Male", new_df$Time, NA)          

stopifnot(all(new_df$Population > 0))

#structure matrices
Q_space <- matrix(0, H$n, H$n)
for (i in 1:H$n) { Q_space[i, i] <- H$nnbs[[i]]; Q_space[i, H$nbs[[i]]] <- -1 }
R.Leroux <- diag(dim(Q_space)[1]) - Q_space

D1 <- diff(diag(ages), differences = 1);       Q_age  <- t(D1) %*% D1
D2 <- diff(diag(timepoints), differences = 1); Q_time <- t(D2) %*% D2

ns <- kronecker.null.space(Q_time,  Q_space); R.st <- ns[[1]]; A_delta.st <- as.matrix(ns[[2]])
ns <- kronecker.null.space(Q_space, Q_age);   R.se <- ns[[1]]; A_delta.se <- as.matrix(ns[[2]])
ns <- kronecker.null.space(Q_age,   Q_time);  R.te <- ns[[1]]; A_delta.te <- as.matrix(ns[[2]])

# cross-basis terms, also with Gender interaction
cb_baseline_terms  <- paste(cb_cols, collapse = " + ")
cb_deviation_terms <- paste(paste0(cb_cols, ":Gender"), collapse = " + ")

#build different models
base_covariates <- paste(
  "Humidity_mean + Elevation +Hot_week+Cold_week+Super_cold_week+Tropical_week+Mild_week+",
  "Increased_risk_humidity + Serious_risk_humidity +",
  "Strong_discomfort_humidity + Severe_malaise_humidity"
)

fixed_a <- paste(cb_baseline_terms, "+ Gender +", base_covariates)
fixed_b <- paste(cb_baseline_terms, "+ Gender +", cb_deviation_terms, "+", base_covariates)
fixed_c <- paste(cb_baseline_terms, "+ Gender +", cb_deviation_terms, "+ factor(Age) +", base_covariates)

re_spatial_temporal <- paste(
  "f(ID, model='generic1', Cmatrix=R.Leroux, constr=TRUE,",
  "  hyper=list(prec=list(prior='loggamma', param=c(1,0.01)), beta=list(prior='logitbeta',param=c(1,1)))) +",
  "f(Time, model='rw1', constr=TRUE)"
)
re_age_rw1 <- "f(Age, model='rw1', constr=TRUE)"
re_age_iid <- "f(Age, model='iid', hyper=list(prec=list(prior='pc.prec', param=c(1, 0.01))))"

#male deviation term strings
re_gender_spatial <- "f(ID_male, model='iid', hyper=list(prec=list(prior='pc.prec', param=c(1, 0.01))))"
re_age_male_iid   <- "f(Age_male, model='iid', hyper=list(prec=list(prior='pc.prec', param=c(1, 0.01))))"

re_time_male_rw1  <- paste0(
  "f(Time_male, model='rw1', constr=TRUE, values=1:", timepoints, ", ",
  "hyper=list(prec=list(prior='pc.prec', param=c(1,0.01))))"
)

re_age_male_rw1 <- paste0(
  "f(Age_male, model='rw1', constr=TRUE, values=1:", ages, ", ",
  "hyper=list(prec=list(prior='pc.prec', param=c(1,0.01))))"
)

re_type4 <- paste(
  "f(ID.prov.age, model='generic0', Cmatrix=R.se, constr=TRUE, extraconstr = list(A=A_delta.se, e=rep(0, dim(A_delta.se)[1]))) +",
  "f(ID.prov.week, model='generic0', Cmatrix=R.st, constr=TRUE, extraconstr = list(A=A_delta.st, e=rep(0, dim(A_delta.st)[1]))) +",
  "f(ID.age.week, model='generic0', Cmatrix=R.te, constr=TRUE, extraconstr = list(A=A_delta.te, e=rep(0, dim(A_delta.te)[1])))"
)

# full-deviation formulas (space + age + time male deviations) ----------
form_rw1_fulldev <- paste(
  "Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_rw1,
  "+", re_type4,
  "+", re_gender_spatial,   # male space deviation
  "+", re_age_male_rw1,     # male age deviation
  "+", re_time_male_rw1     # male time deviation
)

form_iid_fulldev <- paste(
  "Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_iid,
  "+", re_type4,
  "+", re_gender_spatial,
  "+", re_age_male_iid,
  "+", re_time_male_rw1
)

#14 different models 
model_definitions <- list(
  "M1a_Main_AgeRW1"  = paste("Deaths ~", fixed_a, "+", re_spatial_temporal, "+", re_age_rw1),
  "M1a_Main_AgeIID"  = paste("Deaths ~", fixed_a, "+", re_spatial_temporal, "+", re_age_iid),
  "M1a_Type4_AgeRW1" = paste("Deaths ~", fixed_a, "+", re_spatial_temporal, "+", re_age_rw1, "+", re_type4),
  "M1a_Type4_AgeIID" = paste("Deaths ~", fixed_a, "+", re_spatial_temporal, "+", re_age_iid, "+", re_type4),
  "M1b_Main_AgeRW1"  = paste("Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_rw1),
  "M1b_Main_AgeIID"  = paste("Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_iid),
  "M1b_Type4_AgeRW1" = paste("Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_rw1, "+", re_type4),
  "M1b_Type4_AgeIID" = paste("Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_iid, "+", re_type4),
  "M1c_Main"         = paste("Deaths ~", fixed_c, "+", re_spatial_temporal),
  "M1c_Type4"        = paste("Deaths ~", fixed_c, "+", re_spatial_temporal, "+", re_type4),
  
  # shared-baseline + male SPATIAL deviation only
  "M1b_Type4_AgeRW1_MaleDev" = paste("Deaths ~", fixed_b, "+", re_spatial_temporal, "+", re_age_rw1, "+", re_type4, "+", re_gender_spatial),
  "M1c_Type4_MaleDev"        = paste("Deaths ~", fixed_c, "+", re_spatial_temporal, "+", re_type4, "+", re_gender_spatial),
  
  # shared-baseline + FULL male deviation (space + age + time)
  "M1b_Type4_AgeRW1_FullDev" = form_rw1_fulldev,
  "M1b_Type4_AgeIID_FullDev" = form_iid_fulldev
)

# benchmark loop 
results_table <- data.frame()
for (model_name in names(model_definitions)) {
  cat("\n---", model_name, "---\n")
  f_formula <- as.formula(model_definitions[[model_name]])
  gender_type <- if (grepl("M1a", model_name)) "Factor" else "Gender-Temp deviation"
  age_type <- if (grepl("AgeRW1", model_name)) "RW1" else if (grepl("AgeIID", model_name)) "IID" else "Fixed"
  has_t4 <- grepl("Type4", model_name)
  dev_type <- if (grepl("FullDev", model_name)) "space+age+time" else if (grepl("MaleDev", model_name)) "space" else "none"
  
  t0 <- Sys.time()
  res <- tryCatch(
    inla(f_formula, family = "poisson", data = new_df, E = Population,
         control.compute = list(dic = TRUE, waic = TRUE, openmp.strategy = "pardiso"),
         control.predictor = list(compute = FALSE),
         control.inla = list(strategy = "gaussian", int.strategy = "eb"),
         verbose = FALSE),
    error = function(e) { cat("Error:", e$message, "\n"); NULL })
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  if (!is.null(res)) {
    results_table <- rbind(results_table, data.frame(
      Model = model_name, Fixed_Gender = gender_type, Age_Modeling = age_type,
      Has_Type4 = has_t4, Male_Deviation = dev_type,
      DIC = res$dic$dic, pD = res$dic$p.eff,
      WAIC = res$waic$waic, pW = res$waic$p.eff,
      RunTime_Min = round(el / 60, 2)))
  }
}

results_table <- results_table %>%
  mutate(delta_DIC = round(DIC - min(DIC), 2),
         delta_WAIC = round(WAIC - min(WAIC), 2)) %>%
  arrange(WAIC)
print(results_table)

save(results_table, file = "00_model_comparison_with_fulldev_finer_grid.RData")

# full gender deviation models are the best. Refit with config=T and safe
fit_one <- function(form, label) {
  cat("\n=== Fitting", label, "===\n")
  t0 <- Sys.time()
  res <- inla(
    as.formula(form), family = "poisson", data = new_df, E = Population,
    control.family    = list(link = "log"),
    control.compute   = list(dic = TRUE, waic = TRUE, config = TRUE,
                             openmp.strategy = "pardiso"),
    control.predictor = list(compute = TRUE, link = 1),
    control.inla      = list(strategy = "gaussian", int.strategy = "eb"),
    verbose = FALSE
  )
  el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("%s: %.1f min | DIC %.1f | WAIC %.1f\n",
              label, el, res$dic$dic, res$waic$waic))
  res
}

result_fulldev_AgeRW1 <- fit_one(form_rw1_fulldev, "M1b_Type4_AgeRW1_FullDev")
save(result_fulldev_AgeRW1, file = "result_M1b_Type4_AgeRW1_FullDev_finer_grid.RData")

result_fulldev_AgeIID <- fit_one(form_iid_fulldev, "M1b_Type4_AgeIID_FullDev")
save(result_fulldev_AgeIID, file = "result_M1b_Type4_AgeIID_FullDev_finer_grid.RData")

