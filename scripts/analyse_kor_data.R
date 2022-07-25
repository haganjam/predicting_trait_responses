
# Analyse the Korranneberg data data

# load packages
library(dplyr) # data wrangling
library(readr) # read data files
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(stringr) # work with strings
library(lubridate) # work with dates
library(here) # path management
library(vegan)

# load the plotting theme
source(here("scripts/Function_plotting_theme.R"))

# set the seed
set.seed(545983745)

# load the community data
com <- read_csv(here("data/analysis_data/kor_com.csv"))
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

# calculate relative abundance
com1 <- 
  com1 %>%
  group_by(Pool, d1990_d2000) %>%
  mutate(sum_abundance = sum(abundance.m)) %>%
  ungroup() %>%
  mutate(relative_abundance = round( abundance.m/sum_abundance, 5) ) %>%
  select(-sum_abundance)

# read in the .csv with the biomass data
kor_bio <- read_csv(here("data/biomass_conversions/kor_bio.csv"))
head(kor_bio)

# rename the taxon column to species
kor_bio <- 
  kor_bio %>%
  rename(species = taxon)

# join the biomass to the com1 data
com1 <- full_join(com1, kor_bio, by = "species")
View(com1)

# check if the missing biomass values matter
com1 <- 
  com1 %>%
  group_by(Pool, d1990_d2000) %>%
  mutate(exclude = ifelse(any(is.na(biomass_mg) & (relative_abundance > 0.05)), 1, 0)) %>%
  filter(exclude == 0) %>%
  select(-exclude) %>%
  ungroup()
length(unique(com1$Pool))

com1 %>%
  group_by(Pool) %>%
  summarise(check = length(unique(d1990_d2000))) %>%
  View()

# remove pools p4 and p6
com1 <- 
  com1 %>%
  filter(!(Pool %in% c("P4", "P6")))

# remove records without any biomass
com1 <- 
  com1 %>%
  filter(!is.na(biomass_mg))

# calculate biomass for each species
com1 <- 
  com1 %>%
  mutate(biomass_mg = abundance.m*biomass_mg)

# pivot com1 wider again
com1 <- 
  com1 %>%
  select(-n, -abundance.m, -relative_abundance, -life_stage) %>%
  pivot_wider(id_cols = c("Pool", "d1990_d2000"),
              names_from = "species",
              values_from = "biomass_mg")
head(com1)

# create a site information data.frame
site <- com1[, c("Pool", "d1990_d2000")]
head(site)

# create a species abundance data.frame
sp <- com1[, !(names(com1) %in% c("Pool", "d1990_d2000")) ]
head(sp)

# add a column of species richness
site$SR <- apply(sp, 1, function(x) sum(x > 0) )

sp.l <- split(sp, site$Pool)

dom <- 
  lapply(sp.l, function(y) {
    
    x <- apply(y, 1, function(x) x/sum(x))
    
    ra_ext <- x[,1][(x[,1] > 0) & (x[,2] == 0)]
    ra_col <- x[,2][(x[,1] == 0) & (x[,2] > 0)]
    
  return(list("ra_extinct" = ra_ext, "ra_colonise" = ra_col))
  
} )

# calculate the difference in species richness
SR_d <- 
  site %>%
  group_by(Pool) %>%
  summarise(diff_SR = diff(SR))

# plot the change in species richness as a histogram
p1 <- 
  ggplot(data = SR_d,
         mapping = aes(x = diff_SR)) +
  geom_histogram(alpha = 1, colour = "black", fill = "black") +
  ylab("Frequency") +
  xlab("Species richness change") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  theme_meta()
plot(p1)


# contrast regional species richness between the two time-points

# 1990 regional species richness
reg_1990 <- apply(com1[com1$d1990_d2000 == "1990", !(names(com1) %in% c("Pool", "d1990_d2000")) ], 2, sum)
reg_1990 <- sum(ifelse(reg_1990 > 0, 1, 0))
print(reg_1990)

# 2016 regional species richness
reg_2016 <- apply(com1[com1$d1990_d2000 == "2016", !(names(com1) %in% c("Pool", "d1990_d2000")) ], 2, sum)
reg_2016 <- sum(ifelse(reg_2016 > 0, 1, 0))
print(reg_2016)

rbind(reg_1990, reg_2016) %>% View()

# combine into a data.frame
df_reg <- 
  data.frame(year = c("1990", "2016"),
             reg_richness = c(reg_1990, reg_2016))

# plot these results
p2 <- 
  ggplot(data = df_reg,
       mapping = aes(x = year, y = reg_richness)) +
  geom_col(width = 0.25, colour = "black", fill = "black") +
  ylab("Regional species richness") +
  xlab("Year") + 
  geom_hline(yintercept = 30, linetype = "dashed", colour = "red") +
  theme_meta()
plot(p2)

# plot histograms of the relative abundance of species going extinct and colonising
ext_col.df <- 
  rbind(data.frame(ext_col = "Extinctions",
                   ra = unlist(lapply(dom, function(x) x[[1]] ), use.names = FALSE)),
        data.frame(ext_col = "Colonisations",
                 ra = unlist(lapply(dom, function(x) x[[2]] ), use.names = FALSE)) )

# plot these data
p3 <- 
  ggplot(data = ext_col.df %>% filter(ext_col == "Extinctions"),
       mapping = aes(x = ra)) +
  geom_density(fill = "black") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_colour_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  xlab("Relative abundance (1993)") +
  ylab("Density (N = 159)") +
  ggtitle("Extinctions") +
  theme_meta() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
plot(p3)

# plot these data
p4 <- 
  ggplot(data = ext_col.df %>% filter(ext_col == "Colonisations"),
         mapping = aes(x = ra)) +
  geom_density(fill = "black") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_colour_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  xlab("Relative abundance (2016)") +
  ylab("Density (N = 146)") +
  ggtitle("Colonisations") +
  theme_meta() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
plot(p4)


library(ggpubr)
p14 <-
  ggarrange( p1, p2, p3, p4,
           ncol = 2, nrow = 2,
           labels = c("a", "b", "c", "d"),
           font.label = list(size = 11, color = "black", face = "plain", family = NULL))
plot(p14)

# check that we have a figures folder
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# arrange these two plots
ggsave(filename = here("figures/fig_2.png"), p14,
       width = 20, height = 18, units = "cm", dpi = 300)

# calculate total biomass for each pond at the different time points
site$biomass <- apply(sp, 1, function(x) sum(x) )

# calculate the difference in biomass per pond
SR_bio_d <- 
  site %>%
  group_by(Pool) %>%
  summarise(diff_SR = diff(SR),
            diff_biomass = diff(biomass))

cor.test(SR_bio_d$diff_SR, SR_bio_d$diff_biomass)

p5 <- 
  ggplot(data = SR_bio_d,
       mapping = aes(x = diff_SR, y = diff_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, colour = "black", alpha = 0.2) +
  ylab("Change in biomass (mg)") +
  xlab("Change in species richness") +
  theme_meta()
plot(p5)

# check some of the extreme points
SR_bio_d %>%
  filter(diff_biomass < -1000)
  
com1 %>%
  filter(Pool %in% c("P32", "P22", "P33", "P34", "P35")) %>%
  View()

# arrange these two plots
ggsave(filename = here("figures/fig_3.png"), p5,
       width = 10, height = 8, units = "cm", dpi = 450)

### END
