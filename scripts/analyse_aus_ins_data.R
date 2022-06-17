
# Analyse Australia inselberg data

# load packages
library(dplyr) # data wrangling
library(readr) # read data files
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(stringr) # work with strings
library(lubridate) # work with dates
library(here) # path management
library(vegan)

# load the community data
com <- read_csv(here("data/analysis_data/aus_ins_com.csv"))
head(com)

# load the environmental data
env <- read_csv(here("data/analysis_data/aus_ins_env.csv"))
head(env)

### END
