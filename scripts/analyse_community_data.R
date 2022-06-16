
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

# remove the pool 36
com <- 
  com %>%
  filter(Pool != "P36")

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

# pivot com1 wider again
com1 <- 
  com1 %>%
  select(-n) %>%
  pivot_wider(id_cols = c("Pool", "d1990_d2000"),
              names_from = "species",
              values_from = "abundance.m")
head(com1)

# create a site information data.frame
site <- com1[, c("Pool", "d1990_d2000")]
head(site)

# create a species abundance data.frame
sp <- com1[, names(com1) != c("Pool", "d1990_d2000")]
head(sp)

# add a column of species richness
site$SR <- apply(sp, 1, function(x) sum(x > 0) )
View(site)

sp.l <- split(sp, site$Pool)
apply(sp.l[[1]], 1, function(x) x/sum(x))

dom <- 
  lapply(sp.l, function(y) {
  
  # calculate relative abundance
  x <- apply(y, 1, function(x) x/sum(x))
  
  # did any species with a relative abundance of greater than 0.05 go extinct?
  dom_ext <- sum((x[,1] > 0.1) & (x[,2] == 0) )
  
  # what was the relative abundance of species that did go extinct?
  ra_ext <- mean(x[,1][ (x[, 1] > 0 & x[, 2] == 0) ])
  
  # what was the relative abundance of species that colonised?
  ra_col <- mean(x[,2][ (x[, 2] > 0 & x[, 1] == 0) ])
  
  return(data.frame(dom_ext, ra_ext, ra_col))
  
} )

dom <- 
  bind_rows(dom, .id = "Pool") %>%
  arrange(Pool)

SR_d <- lapply(split(site, site$Pool), function(x) data.frame(diff_SR = diff(x$SR)) )
SR_d <- 
  bind_rows(SR_d, .id = "Pool") %>%
  arrange(Pool)

full_join(SR_d, dom) %>%
  ggplot(data = .,
         mapping = aes(x = diff_SR, y = dom_ext)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_bw()


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







