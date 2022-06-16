
# Predicting shifts in trait distributions due to environmental change

# Parameterise the evaporation term C

# load relevant libraries
library(here)
library(readr)
library(dplyr)
library(lubridate)

rm(list = ls())

source(here("scripts/hydrological_model_function.R"))

# load the daily precipitation data
dail.prec <- read_csv(here("data/hydrological_model/daily_precipitation_data_raw.csv"))
nrow(dail.prec)

# get the relevant dates for the evaporation data
dail.evap <- 
  dail.prec %>%
  filter(yyyy_mm_dd >= as_date("2005-10-01") &  yyyy_mm_dd <= as_date("2005-11-28"))
# View(dail.evap)

# read pool-specific parameters
pool_par <- read_csv(here("data/hydrological_model/pool_specific_parameters_raw.csv"))

# load the calibration data
cal_dat <- read_csv(here("data/hydrological_model/cleaned_calibration_data.csv"))
cal_dat <- cal_dat[, -c(5) ]
cal_dat <- 
  cal_dat %>%
  distinct()
View(cal_dat[!complete.cases(cal_dat), ])
cal_dat <- cal_dat[complete.cases(cal_dat), ]
cal_dat %>%
  group_by(pool_id) %>%
  summarise(n = n()) %>%
  View()

# check if all pools have enough calibration data
cal_dat %>%
  group_by(pool_id) %>%
  mutate(obs_depth_mm = if_else(obs_depth_mm > 0, 1, 0)) %>%
  summarise(obs_depth_mm = sum(obs_depth_mm)) %>%
  View()

# pool 36 only has one non-zero datapoint so we will have to derive the C value in a different way
cal_dat <- 
  cal_dat %>%
  filter( pool_id != 36 )

# add the evaporation data to the calibration data
cal_dat <- left_join(cal_dat, dail.evap[, c("yyyy_mm_dd", "daily_evaporation_mm_day")], by = "yyyy_mm_dd")
# View(cal_dat)
sum(!complete.cases(cal_dat))
View(cal_dat[!complete.cases(cal_dat), ])
View(cal_dat)
cal_dat <- cal_dat[complete.cases(cal_dat), ]

# split the calibration data by pool
cal_split <- split(cal_dat, cal_dat$pool_id)

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
  
  C_try <- seq(0, 10, 0.01)
  cor.out <- vector("double", length = length(C_try))
  for (j in 1:length(C_try)) {
    
    # parameterise with training data
    x <- pool_n_cal
    
    if ( sum( ifelse(x$obs_depth_mm > 0, 1, 0) ) < 3 ) {
      
      C_try <- NA
      cor.out <- NA
      
    } else {
      
      Ev_vec <- (x$daily_evaporation_mm_day * C_try[j] * pool_n_par$area_m2)/surface_CI
      P_R_vec <- x$precipitation_mm * pool_n_par$runoff_factor_R
      
      mod.depth <- hydro_model(P_R_vec = P_R_vec, Ev_vec = Ev_vec, 
                               min_depth = pool_n_par$min_depth_mm, max_depth = pool_n_par$max_depth_mm)
      
      cor.out[j] <- cor(mod.depth, x$obs_depth_mm)
      
    }
    
  }
  
  # get the maximum correlation
  max_cor <- which(cor.out == max(cor.out, na.rm = TRUE))
  C.optim <- C_try[max_cor]
  pearson_r <- cor.out[max_cor]
  
  list.out <- 
    list(fitted_C = C.optim,
         pearson_r = pearson_r)
  
  pool.C[[k]] <- list.out
  
}

pool_par$est_C_daily_evap <- c( unlist(lapply(pool.C, function(x) {x[[1]]})), NA )
pool_par$est_C_r <- c( unlist(lapply(pool.C, function(x) {x[[2]]})), NA )

# see how the C values correlate?
plot(pool_par$evap_factor_C, pool_par$est_C_daily_evap)
cor(pool_par$evap_factor_C, pool_par$est_C_daily_evap, use = "pairwise.complete.obs")

### END
