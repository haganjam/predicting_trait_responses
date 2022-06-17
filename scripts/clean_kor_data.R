
# Process the commmunity data and the environmental data

# load packages
library(dplyr) # data wrangling
library(readr) # read data files
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(stringr) # work with strings
library(lubridate) # work with dates
library(here) # path management
library(readxl)

# write a function to load all sheets from an excel file
# https://www.geeksforgeeks.org/how-to-read-a-xlsx-file-with-multiple-sheets-in-r/
multiplesheets <- function(fname, nmax, skip = 0) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x, 
                                                          n_max = nmax,
                                                          skip = skip))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# use the function to load the community data
com <- multiplesheets(fname = here("data/Korranneberg_data/community_data/community_data_Korranneberg_1998_2018.xlsx"),
                      nmax = 36)

# get the date and days of inundation
Date <- as.Date(c("1993/10/14", "1993/11/08", "1994/01/06", "1994/02/19", "2016/05/15", "2016/12/15", "2016/02/15"), 
                tryFormats = "%Y/%m/%d") 
Days_inun <- c(12, 37, 96, 140, NA, NA, NA)

# add the date and days since inundation for all the time-points
for(i in 1:length(com)) {
  
  com[[i]]$Date <- Date[i]
  com[[i]]$Days_inun <- Days_inun[i]
  
}

# do they all have the same names?
lapply(com, function(x) length(names(x)) )
lapply(com, function(x) (names(x)) )

# bind into one large data.frame
com <- 
  bind_rows(com) %>%
  select(Pool = ...1, Date, Days_inun, Branchipodopsis:Alfa)


# make a data.frame for the biomass conversions
bio_sp <- data.frame(taxon = names(com)[-c(1,2,3)],
                     life_stage = NA)

# write this into a .csv file
write_csv(bio_sp, here("data/biomass_conversions/kor_bio.csv"))

# write the community data into a .csv file
write_csv(x = com,
          file = here("data/analysis_data/kor_com.csv"))


# load the environmental data
env <- 
  readxl::read_excel(here("data/Korranneberg_data/environmental_variables/environmental_data_Korranneberg.xlsx"))
names(env)

# rename the pool variable and convert to same format as the community data
env <- 
  env %>%
  rename(Pool = Poolname) %>%
  mutate(Pool = gsub(pattern = "Kor", replacement = "P", x = Pool))

# remove columns that do not have any data
env <- env[, unlist( lapply(env, function(x) nrow(env) != sum(is.na(x))) ) ]
head(env)

# check how many NAs there are for the different variables
lapply(env, function(x) sum(is.na(x)))

# write the environmental data into a .csv file
write_csv(x = env,
          file = here("data/analysis_data/kor_env.csv"))

### END
