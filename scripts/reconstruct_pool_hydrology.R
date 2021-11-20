
# Predicting shifts in trait distributions due to environmental change

# Reconstruct the hydrology of the Koranneberg pools

# load relevant libraries
library(here)
library(dplyr)
library(readr)

rm(list = ls())

# read the relevant data

# load the daily precipitation data
dail.prec <- read_csv(here("data/hydrological_model/daily_precipitation_data_raw.csv"))
nrow(dail.prec)
head(dail.prec)

# check for NAs in the daily rainfall and precipitation data
dail.prec %>%
  filter(is.na(daily_evaporation_mm_day) | is.na(precipitation_mm)) %>%
  View()

# load monthly evaporation data
mont.evap <- read_csv(here("data/hydrological_model/monthly_evaporation_averages_raw.csv"))

# read pool-specific parameters
pool_par <- read_csv(here("data/hydrological_model/pool_specific_parameters_raw.csv"))

# add the surface evaporation factor
surface_CI <- 1.1442

# convert the pool parameters to a list
pool_list <- split(pool_par, pool_par$pool_id)

# subset out the first pool
pool_n <- pool_list[[1]]

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

for (i in 2:length(depth) ) {
  
  # the precipitation or evaporation data is missing then use the previous value for depth
  if (is.na(P_R_vec[i]) | is.na(Ev_vec[i]) ) {
    
    x <- depth[i-1]
    
  } else {
    
    # calculate the water balance at t:
    # depth at t-1 + (P_R i.e. rainfall) - (Ev i.e. evaporation)
    x <- depth[i-1] + P_R_vec[i] - Ev_vec[i]
    
    # if water balance exceeds maximum pool depth, then use the max pool depth
    x <- ifelse(x > pool_n$max_depth_mm, pool_n$max_depth_mm, x)
    
    # if water balance is less than minimum depth, then use the min pool depth
    x <- ifelse(x < pool_n$min_depth_mm, pool_n$min_depth_mm, x)
    
  }
  
  depth[i] <- x
  
}

# test if the correct number of depth measurements were produced
length(depth) == nrow(pool_prec)

# add the depth data to the pool_prec data
pool_prec$depth_mm <- depth

# view the data
View(pool_prec)
View(pool_prec[(pool_prec$year == 1991 ), ])






