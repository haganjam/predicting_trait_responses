
# Analyse the community data

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
com <- read_csv(here("data/analysis_data/com_dat.csv"))
head(com)

com1 <- 
  com %>%
  mutate(d1990_d2000 = if_else(Date < as.Date("2000/10/14", "%Y/%m/%d"), "1990", "2016")) %>%
  group_by(Pool, d1990_d2000) %>%
  mutate(sample = 1:n()) %>%
  arrange(Pool, d1990_d2000, sample) %>%
  select(Pool, d1990_d2000, sample, all_of(names(com)[-c(1, 2, 3)])) %>%
  ungroup()

com1 <- 
  com1 %>%
  group_by(Pool, d1990_d2000) %>%
  sample_n(size = 3) %>%
  select(-sample) %>%
  pivot_longer(cols = names(com)[-c(1, 2, 3)],
               names_to = "species",
               values_to = "abundance") %>%
  group_by(Pool, d1990_d2000, species) %>%
  summarise(abundance.m = mean(abundance, na.rm = TRUE),
            n = n()) %>%
  ungroup()

# check if any pool has more than three samples
range(com1$n)

# create a site information data.frame
site <- com1[, c("Pool", "Date", "Days_inun")]
head(site)

# create a species abundance data.frame
sp <- com[, names(com) != c("Pool", "Date", "Days_inun")]
head(sp)

# remove rows without zero total species abundance
x <- apply(sp, 1, function(x) sum(x)) != 0
sp <- sp[x, ]
site <- site[x, ]

# add a column of species richness
site$SR <- apply(sp, 1, function(x) sum(x > 0) )

# add a column for rarefied species richness
site$SR_rare <- rarefy(x = sp, sample = 10)

# check correlation between species richness and rarefied species richness
plot(site$SR, site$SR_rare)

# plot changes in species richness through time by pool
ggplot(data = site %>% filter(Date < as.Date("2000/10/14", "%Y/%m/%d") ),
       mapping = aes(x = Date,
                     y = SR,
                     colour = Pool)) +
  geom_point() +
  geom_line() +
  theme_bw()

site %>%
  mutate(d1990_d2000 = if_else(Date < as.Date("2000/10/14", "%Y/%m/%d"), "1990", "2016")) %>%
  group_by(d1990_d2000, Pool) %>%
  summarise(SR = mean(SR, na.rm = TRUE), .groups = "drop") %>%
  group_by(Pool) %>%
  summarise(SR_diff = diff(SR), .groups = "drop") %>%
  ggplot(data = .,
         mapping = aes(x = SR_diff)) +
  geom_histogram() +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  xlab("Delta: species richness") +
  theme_bw()

site[site$Pool == "P21", ]
View(sp[site$Pool == "P21", ])







