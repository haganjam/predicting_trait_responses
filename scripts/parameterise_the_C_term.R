
# Predicting shifts in trait distributions due to environmental change

# Parameterise the evaporation term C

# load relevant libraries
library(here)
library(readr)
library(dplyr)

source(here("scripts/hydrological_model_function.R"))

# load the daily precipitation data
dail.prec <- read_csv(here("data/hydrological_model/daily_precipitation_data_raw.csv"))
nrow(dail.prec)

# get the relevant dates for the evaporation data
dail.evap <- 
  dail.prec %>%
  filter(yyyy_mm_dd >= as_date("2005-10-01") &  yyyy_mm_dd <= as_date("2005-11-28"))
View(dail.evap)

# read pool-specific parameters
pool_par <- read_csv(here("data/hydrological_model/pool_specific_parameters_raw.csv"))

# load the calibration data
cal_dat <- read_csv(here("data/hydrological_model/cleaned_calibration_data.csv"))
cal_dat <- cal_dat[, -c(5) ]
cal_dat[complete.cases(cal_dat), ]

# add the evaporation data to the calibration data
cal_dat <- left_join(cal_dat, dail.evap[, c("yyyy_mm_dd", "daily_evaporation_mm_day")], by = "yyyy_mm_dd")
View(cal_dat)
sum(!complete.cases(cal_dat))
cal_dat <- cal_dat[complete.cases(cal_dat), ]

# split the calibration data by pool
cal_split <- split(cal_dat, cal_dat$pool_id)
cal_split

# add the surface evaporation factor
surface_CI <- 1.1442


### iterate over each pool

# output should be
# 1. the fitted evaporation parameter (C) 
# 2. the r value between the observed and modelled water level using the fitted C parameter on the testing data

# for each pool
pool.C <- vector("list", length = length(cal_split))
for (k in 1:length(cal_split)) {
  
  pool_n_cal <- cal_split[[k]]
  pool_n_par <- pool_par[pool_par$pool_id == k, ]
  
  # add the relevant water balance terms
  
  # split the data into a test and training set
  n.rows <- nrow(pool_n_cal)
  train.dat <- 1:round( (n.rows/2), 0 )
  test.dat <- ( max(train.dat) + 1 ):n.rows
  
  C_try <- seq(0, 10, 0.01)
  cor.out <- vector("double", length = length(C_try))
  for (j in 1:length(C_try)) {
    
    # parameterise with training data
    x <- pool_n_cal[train.dat, ]
    
    Ev_vec <- (x$daily_evaporation_mm_day * C_try[j] * pool_n_par$area_m2)/surface_CI
    P_R_vec <- x$precipitation_mm * pool_n_par$runoff_factor_R
    
    mod.depth <- hydro_model(P_R_vec = P_R_vec, Ev_vec = Ev_vec, 
                             min_depth = pool_n_par$min_depth_mm, max_depth = pool_n_par$max_depth_mm)
    
    cor.out[j] <- cor(mod.depth, x$obs_depth_mm)
    
  }
  
  C.optim <- C_try[which(cor.out == max(cor.out, na.rm = TRUE))]
  
  # use the fitted evaporation term on the rest of the data to test model performance
  z <- pool_n_cal[test.dat, ]
  
  Ev_vec <- (z$daily_evaporation_mm_day * C.optim * pool_n_par$area_m2)/surface_CI
  P_R_vec <- z$precipitation_mm * pool_n_par$runoff_factor_R
  
  mod.depth <- hydro_model(P_R_vec = P_R_vec, Ev_vec = Ev_vec, 
                           min_depth = pool_n_par$min_depth_mm, max_depth = pool_n_par$max_depth_mm)
  
  list.out <- 
    list(fitted_C = C.optim,
               pearson_r = cor(mod.depth, z$obs_depth_mm) )
  
  pool.C[[k]] <- list.out
  
}

pool.C


