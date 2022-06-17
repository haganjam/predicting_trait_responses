
# Analyse the Australia Inselberg data

# load packages
library(dplyr) # data wrangling
library(readr) # read data files
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(here) # path management
library(stringr)
library(vegan)

# load the community data
com <- read_csv(here("data/australia_inselbergs/reg_spp_site_dat.csv"))
head(com)
View(com)

# Caeculidae column has a problem
com$Caeculidae[1] <- "0"
com$Caeculidae <- as.double(com$Caeculidae)

# check the structure of the different variables
str(com)

# check the species names
name.x <- names(com)[-1]
name.x <- gsub(pattern = "_", " ", x = name.x)

high_tax <- word(name.x, 1, 1)
high_tax[45:62] <- word(name.x[45:62], 2, 2) 

# replace weevil with superfamily
high_tax[high_tax == "weevil"] <- "Curculionoidea"

# replace Turb with Turbellaria
high_tax[high_tax == "Turb"] <- "Turbellaria"

# check 73 to 75
high_tax[73:75] <- word(name.x[73:75], 2, 2) 

# check 88: add pupae later
name.x[88]
high_tax[88] <- "Orthocladiinae"

# replace mites with Acari
high_tax[high_tax %in% c("miteA1", "miteA2")] <- "Acari"

# second level
low_tax <- word(name.x, 2, 2)

# remove the second words that we know are wrong
low_tax[c(45:62, 73:75, 88)] <- NA

# remove the sp names
low_tax[grepl(pattern = "sp", x = low_tax)] <- NA

# replace other missing data with NA
low_tax[c(63:69, 93, 94) ] <- NA

# wrangle the data to get a set of appropriate names that can be used for biomass conversions
com <- 
  com %>%
  pivot_longer(cols = names(com)[-1],
               names_to = "species",
               values_to = "abundance") %>%
  rename(site = Species)

taxon <- paste(high_tax, low_tax, sep = " ")
taxon <- gsub(pattern = "\ NA", replacement = "", taxon)

# add this taxon name to the data.frame
com$taxon <- rep(taxon, length(unique(com$site)) )

# reorder the columns
com <- 
  com %>%
  select(site, species, taxon, abundance)
head(com)

# make a data.frame to get the biomass values for these different taxa
bio_sp <- 
  com %>%
  select(taxon) %>%
  distinct() %>%
  mutate(life_stage = NA)

bio_sp[bio_sp$taxon == "Orthocladiinae", ]$life_stage <- "pupae"
View(bio_sp)

# write this into a .csv file
write_csv(x = bio_sp, here("data/biomass_conversions/aus_insel_bio.csv"))


# load the environmental data
env <- read_csv(here("data/australia_inselbergs/env_data.csv"))
head(env)


