# In this file: spatio-temporal SPDE downscaling at daily resolution, blocked by year.
# Variables: Precip_sum, Temp mean/min/ max, Humidity mean
# Time horizon: 2002-2019
# ============================================================================
library(raster)
library(sp)
library(dplyr)
library(tidyverse)
library(sf)
library(INLA)
library(geodata)
library(terra)
library(future)
library(future.apply)

load("01_Austrian_districts.RData")  
load("01_raw_data.R")

crs_planar   <- 31287
grid_step_km <- 7           # grid point every 7km
PRECIP_SHIFT <- 0.05         # mm added to precip before Gamma; removed after
N_WORKERS    <- 4           # tune: workers * threads_per_inla <= cores,

INLA_THREADS <- "2:1"       # threads per inla() call (outer:inner)
OUTDIR       <- "02b_jobs_all_districts_finer_grid_spatial"  # per-job output folder
dir.create(OUTDIR, showWarnings = FALSE)

continuous_vars <- c("Temp_mean", "Temp_min", "Temp_max",
                     "Humidity_mean", "Precip_sum")

# ---- prep station daily frame ---------------------------------------------
weather_data_all <- weather_data_all[, -c(9, 13)]
colnames(weather_data_all)[7] <- "Date"
weather_data_all$Year <- lubridate::isoyear(weather_data_all$Date)
weather_data_all <- weather_data_all %>%
  filter(Year >= 2002, Year < 2020)



# ---- geometry: districts, boundary, station coords (degrees -> km), mesh --------
districts_sf <- st_transform(Austrian_districts_combined, crs_planar)
boundary     <- st_union(districts_sf)
boundary_seg <- inla.sp2segment(as(boundary, "Spatial"))
boundary_seg$loc <- boundary_seg$loc / 1000

stations_sf <- weather_data_all %>%
  distinct(Station, Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs_planar)
station_xy_km <- st_coordinates(stations_sf) / 1000
rownames(station_xy_km) <- stations_sf$Station

mesh <- inla.mesh.2d(
  loc = station_xy_km, boundary = boundary_seg,
  max.edge = c(15, 50), cutoff = 10, offset = c(15, 50)
)
cat("Mesh nodes:", mesh$n, "\n")

# ---- grid with elevation as covariate ------------------------
grid_pts <- st_make_grid(boundary, cellsize = grid_step_km * 1000, what = "centers")
grid_pts <- grid_pts[st_within(grid_pts, boundary, sparse = FALSE)]
grid_sf  <- st_as_sf(grid_pts)
grid_sf <- st_set_geometry(grid_sf, "geometry")   # rename x -> geometry

grid_sf$District <- st_join(grid_sf, districts_sf, join = st_within)$District
grid_sf  <- grid_sf[!is.na(grid_sf$District), ]

# st_point_on_surface returns a point guaranteed to lie inside each polygon
dist_pts <- st_point_on_surface(districts_sf)
dist_pts <- st_sf(District = districts_sf$District,
                  geometry  = st_geometry(dist_pts))

grid_xy_km <- st_coordinates(grid_sf) / 1000
n_grid <- nrow(grid_xy_km)
cat("Grid points:", n_grid, "\n")

grid_sf <- grid_sf %>%
  left_join(
    Austrian_districts_combined %>% st_drop_geometry(), 
    by = "District"
  )
stopifnot("Elevation" %in% names(grid_sf), all(!is.na(grid_sf$Elevation)))

# ---- SPDE------------------------------------------
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(70, 0.5),  # P(range < 50 km) = 0.05
  prior.sigma = c(5, 0.01)    # P(sigma > 5) = 0.01
)

# ---- fit one variable within one year, output: per-job ------------
# To reduce computation: Fit the spatio-temporal SPDE on observations only
# (no prediction rows in the stack), then project the posterior field onto the
# grid day-by-day via posterior sampling. 

fit_var_year_daily <- function(var, yr, n_samples = 100, save_grid = FALSE) {
  
  outfile <- file.path(OUTDIR, sprintf("%s_%d.rds", var, yr))
  if (file.exists(outfile)) return(outfile)          # resume
  
  # --- 1. SET MODEL FAMILY & LINK -------------------------------------------
  family <- if (var == "Precip_sum") "gamma" else "gaussian"
  link   <- if (var == "Precip_sum") "log"   else "default"
  
  dat <- weather_data_all %>%
    filter(lubridate::isoyear(Date) == yr, !is.na(.data[[var]])) %>%
    dplyr::select(Station, Date, Elevation, all_of(var))
  
  if (nrow(dat) == 0) return(NA_character_)
  
  day_key <- dat %>% distinct(Date) %>% arrange(Date) %>% mutate(d = row_number())
  D_n <- nrow(day_key)
  dat <- dat %>% left_join(day_key, by = "Date")
  dat$x <- station_xy_km[dat$Station, 1]
  dat$y <- station_xy_km[dat$Station, 2]
  dat$Elevation<-dat$Elevation/1000
  
  # If humidity is passed, transform observations to logit scale before fitting
  if (var == "Humidity_mean") {
    h_prop <- dat[[var]] / 100
    h_prop <- pmin(pmax(h_prop, 0.001), 0.999) # Guard against 0 or 1
    yobs   <- log(h_prop / (1 - h_prop))      # Logit scale
  } else {
    yobs   <- dat[[var]]
  }
  
  if (family == "gamma") {
    yobs <- yobs + PRECIP_SHIFT
    if (any(yobs <= 0, na.rm = TRUE))
      stop("Non-positive ", var, " year ", yr, " after shift.")
  }
  
  field_index <- inla.spde.make.index("field", n.spde = spde$n.spde, n.group = D_n)
  
  
  dat$sin_day <- sin(2 * pi * dat$d / 365.25)
  dat$cos_day <- cos(2 * pi * dat$d / 365.25)
  
  # observation stack
  A_obs <- inla.spde.make.A(mesh = mesh, loc = cbind(dat$x, dat$y),
                            group = dat$d, n.group = D_n)
  stk_obs <- inla.stack(
    tag = "obs", data = list(y = yobs), A = list(A_obs, 1),
    effects = list(field = field_index,
                   data.frame(b0 = 1, Elevation = dat$Elevation,
                              sin_day              = dat$sin_day,
                              cos_day              = dat$cos_day,
                              day_id       = dat$d)  # 1..365)
  ))
  
  # fixed effect priors depending on response var
  is_temp <- grepl("temp|Temp", var, ignore.case = TRUE)
  
  if (is_temp) {
    mean_obs <- mean(yobs, na.rm = TRUE)
    cf_mean <- list(b0 = mean_obs, Elevation = -6.5)
    cf_prec <- list(
      b0           = 0.001,  # Free intercept (very weak constraint)
      Elevation = 25.0    # Informative lapse rate constraint (-6.5 ± 0.4 °C/km)
    )
  } else if (var == "Humidity_mean") {
    # For Humidity (logit scale): elevation effect is weak/zero on logit scale
    cf_mean  <- list(b0 = mean(yobs, na.rm = TRUE), Elevation = 0)
    cf_prec  <- list(b0 = 0.01,                     Elevation = 0.1) # Soft prior
  } else {
    # Default fallback (e.g. Precipitation on log scale)
    cf_mean  <- list(b0 = 0, Elevation = 0)
    cf_prec  <- list(b0 = 0.001, Elevation = 0.01)
  }
  
  fit<- inla(
    y ~  Elevation + -1 + b0 +
      f(field, model = spde)+f(day_id, model= "rw2",constr= TRUE,hyper= list(prec = list(prior = "pc.prec", param = c(3, 0.01)))),
    data = inla.stack.data(stk_obs),
    family = family,
    control.fixed     = list(mean = cf_mean, prec = cf_prec), 
    control.family    = list(control.link = list(model = link)),
    control.predictor = list(A = inla.stack.A(stk_obs), compute = FALSE, link = 1),
    control.compute   = list(config = T, dic=T),
    control.inla      = list(strategy = "gaussian", int.strategy = "eb"),
    num.threads       = INLA_THREADS,
    verbose           = F
  )
  
  # posterior samples 
  samp <- inla.posterior.sample(n_samples, fit, num.threads = INLA_THREADS)
  
  cn         <- rownames(samp[[1]]$latent)
  field_rows <- grep("^field:", cn)
  b0_row     <- grep("^b0:",        cn)
  elev_row   <- grep("^Elevation:", cn)
  day_rows   <- grep("^day_id:", cn) 
  n_spde     <- spde$n.spde
  
  
  field_mat <- sapply(samp, function(s) s$latent[field_rows, 1])
  b0_vec    <- sapply(samp, function(s) s$latent[b0_row,   1])
  elev_vec  <- sapply(samp, function(s) s$latent[elev_row, 1])
  day_mat   <- sapply(samp, function(s) s$latent[day_rows,   1]) 
  
  A_grid_space <- inla.spde.make.A(mesh = mesh, loc = grid_xy_km)
  grid_elev    <- grid_sf$Elevation
  
  inv_link <- if (link == "log") exp else identity
  
  # project per day and summarize to district mean + sd 
  out_list <- vector("list", D_n)
  
  
  # Pre-compute spatial component outside the loop
  eta_space <- as.matrix(A_grid_space %*% field_mat) 
  
  for (dd in seq_len(D_n)) {
    
    eta <- eta_space 
    
    # Add effects per sample
    eta <- sweep(eta, 2, b0_vec, `+`)               # + intercept
    eta <- eta + outer(grid_elev, elev_vec)          # + elevation
    eta <- sweep(eta, 2, day_mat[dd, ], `+`)          # + rw2 daily effect
    
    if (var == "Humidity_mean") {
      resp <- (1 / (1 + exp(-eta))) * 100
    } else if (var == "Precip_sum") {
      resp <- exp(eta) - PRECIP_SHIFT
      resp <- pmax(resp, 0)
    } else {
      resp <- inv_link(eta)
    }
    if (family == "gamma") resp <- pmax(resp, 0)
    
    gmean <- rowMeans(resp)
    gsd   <- sqrt(pmax(rowMeans(resp^2) - gmean^2, 0))   # pmax guards tiny neg from rounding
    
    out_list[[dd]] <- data.frame(
      District = grid_sf$District,
      x = grid_xy_km[, 1], y = grid_xy_km[, 2],
      Day = dd, gmean = gmean, gsd = gsd
    )
  }
  res_df <- bind_rows(out_list)
  saveRDS(res_df, file = outfile)
  
  return(outfile)
  
}

years <- sort(unique(weather_data_all$Year))
jobs  <- expand.grid(var = continuous_vars, yr = years, stringsAsFactors = FALSE)

cat("Total jobs:", nrow(jobs))   

# ---- run in parallel ------------------------------------------------------
plan(multicore, workers = N_WORKERS)

job_files <- future_lapply(seq_len(nrow(jobs)), function(i) {
  tryCatch(
    fit_var_year_daily(jobs$var[i], jobs$yr[i], save_grid = jobs$save_grid[i]),
    error = function(e) {
      message(sprintf("FAILED %s %d: %s",
                      jobs$var[i], jobs$yr[i], conditionMessage(e)))
      NA_character_
    }
  )
}, future.seed = TRUE)


# collect files -> district-daily wide -------------------------
done <- unlist(job_files)
done <- done[!is.na(done) & file.exists(done)]
cat("Completed jobs:", length(done), "of", nrow(jobs), "\n")

district_daily_long <- bind_rows(lapply(done, function(f) {
  df <- readRDS(f)
  
  file_name <- basename(f)
  
  # Extract year 
  yr_val <- as.integer(str_extract(file_name, "\\d{4}(?=\\.rds$)"))
  
  # Extract variable name 
  var_val <- sub("_[0-9]{4}\\.rds$", "", file_name)
  
  # Append columns
  df$Variable <- var_val
  df$Year     <- yr_val
  
  # Ensure standard column names if saved as gmean / gsd
  if ("gmean" %in% names(df)) df <- rename(df, mean = gmean)
  if ("gsd"   %in% names(df)) df <- rename(df, sd   = gsd)
  
  return(df)
}))


library(lubridate)

district_daily <- district_daily_long %>%
  # Reconstruct the true Date using ISO week logic
  mutate(
    iso_week1_monday = floor_date(make_date(Year, 1, 4), unit = "week", week_start = 1),
    Date             = iso_week1_monday + days(Day - 1)
  ) %>%
    select(-iso_week1_monday) %>%
    select(District, Date, Year, Day, x, y, Variable, mean, sd) %>%
  pivot_wider(
    id_cols     = c(District, Date, Year, Day, x, y),
    names_from  = Variable,
    values_from = c(mean, sd),
    names_glue  = "{Variable}_{.value}"
  ) %>%
  
  # Clean column names (e.g., Temp_mean_mean -> Temp_mean)
  rename_with(~ sub("_mean$", "", .x), ends_with("_mean"))

save(district_daily, Austrian_districts_combined,
     file = "01_district_daily_downscaled_finer_grid.RData")
