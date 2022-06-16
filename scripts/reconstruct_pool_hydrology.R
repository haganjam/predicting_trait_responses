
# Predicting shifts in trait distributions due to environmental change

# Reconstruct the hydrology of the Koranneberg pools

# next steps

# calculate some of those summary variables for Korranneberg hydroperiods

# load relevant libraries
library(here)
library(dplyr)
library(readr)
library(lubridate)

rm(list = ls())

# load important functions
source(here("scripts/hydrological_model_function.R"))

# read the relevant data

# load the daily precipitation data
dail.prec <- read_csv(here("data/hydrological_model/daily_precipitation_data_raw.csv"))
nrow(dail.prec)
head(dail.prec)
# View(dail.prec)

# check for NAs in the daily rainfall and precipitation data
dail.prec %>%
  filter(is.na(daily_evaporation_mm_day) | is.na(precipitation_mm)) %>%
  View()

# what can we do about the NAs?
# think about this...

# load monthly evaporation data
mont.evap <- read_csv(here("data/hydrological_model/monthly_evaporation_averages_raw.csv"))
mont.evap

# read pool-specific parameters
pool_par <- read_csv(here("data/hydrological_model/pool_specific_parameters_raw.csv"))

# load the calibration data
cal_dat <- read_csv(here("data/hydrological_model/cleaned_calibration_data.csv"))
cal_dat[complete.cases(cal_dat), ]

# add the surface evaporation factor
surface_CI <- 1.1442

# compare precipitation from the calibration data (measured with rain guage on site) and raw data from weather station
prec.cal <- 
  cal_dat %>%
  filter(pool_id == 1) %>%
  filter(yyyy_mm_dd >= as_date("2005-10-01") &  yyyy_mm_dd <= as_date("2005-11-28")) %>%
  pull(precipitation_mm)

prec.ws <- 
  dail.prec %>%
  filter(yyyy_mm_dd >= as_date("2005-10-01") &  yyyy_mm_dd <= as_date("2005-11-28")) %>%
  pull(precipitation_mm)

length(prec.cal) == length(prec.ws)
plot(prec.ws[!is.na(prec.ws)], prec.cal[!is.na(prec.ws)])
cor(prec.ws[!is.na(prec.ws)], prec.cal[!is.na(prec.ws)])^2

# convert the pool parameters to a list
pool_list <- split(pool_par, pool_par$pool_id)

# apply over all pools here
hydro.out <- 
  
  lapply(pool_list, function(pool) {
  
  # subset out the first pool
  pool_n <- pool
  
  # copy the dail.prec data to be modified
  pool_prec <- dail.prec
  
  # add relevant variables to the pool_prec data
  pool_prec <- 
    pool_prec %>%
    mutate(P_R = (precipitation_mm)*pool_n$runoff_factor_R,
           Ev = (daily_evaporation_mm_day*pool_n$evap_factor_C*pool_n$area_m2)/surface_CI)
  
  # create an empty vector matching the number of rows for the rainfall data
  depth <- vector(mode = "double", length = nrow(pool_prec))
  P_R_vec <- pool_prec$P_R
  Ev_vec <- pool_prec$Ev
  
  # use the hydrological model without overlow to calculate depth
  depth <- hydro_model(P_R_vec = P_R_vec, Ev_vec = Ev_vec, 
                       min_depth = pool_n$min_depth_mm, max_depth = pool_n$max_depth_mm)
  
  # test if the correct number of depth measurements were produced
  length(depth) == nrow(pool_prec)
  
  # test if all depths are not NAs
  any(is.na(depth))
  
  # add the depth data to the pool_prec data
  pool_prec$depth_mm <- depth
  
  # use the reconstructed depth data to calculate hydroperiods
  pool_prec$pool_id <- pool_n$pool_id
  
  return(pool_prec)
  
})

# load the calibration data
cal_dat <- read_csv(here("data/hydrological_model/cleaned_calibration_data.csv"))
cal_dat <- cal_dat[, -c(5) ]
cal_dat <- 
  cal_dat %>%
  distinct()
cal_dat <- cal_dat[complete.cases(cal_dat), ]
cal_dat <- split(cal_dat, cal_dat$pool_id)

# check if we can get the observed depths based on the weather station data
# for the empirically measured pools
test_cor <- mapply(function(x, y) {
  
  z <- left_join(x, y, by = c("yyyy_mm_dd"))
  
  cor(z$depth_mm, z$obs_depth_mm)
  
}, cal_dat, hydro.out)

hist(test_cor)
mean(test_cor, na.rm = TRUE)
sd(test_cor, na.rm = TRUE)

# that's actually not a bad average correlation

### END
