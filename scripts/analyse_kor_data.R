
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

sp.l <- split(sp, site$Pool)
apply(sp.l[[1]], 1, function(x) x/sum(x))

dom <- 
  lapply(sp.l, function(y) {
    
    x <- apply(y, 1, function(x) x/sum(x))
  
    # did any species with a relative abundance of greater than 0.10 go extinct?
    thresh <- c(0.05, 0.1, 0.5, 0.7)
    
    n_ext <- 
      sapply(thresh, function(z) {
        
        sum((x[,1] > z) & (x[,2] == 0) )
        
      })
    
    dom_ext <- data.frame(thresh = thresh,
                          n_ext = n_ext)
    
    ra_ext <- x[,1][(x[,1] > 0) & (x[,2] == 0)]
    ra_col <- x[,2][(x[,1] == 0) & (x[,2] > 0)]
    
  return(list(dom_ext, "ra_extinct" = ra_ext, "ra_colonise" = ra_col))
  
} )

dom.df <- 
  lapply(dom, function(x) x[[1]] ) %>%
  bind_rows(., .id = "Pool") %>%
  arrange(Pool)
View(dom.df)

# calculate the difference in species richness
SR_d <- 
  site %>%
  group_by(Pool) %>%
  summarise(diff_SR = diff(SR))

# join the SR_d to the extinctino data
dom.df <- full_join(SR_d, dom.df, by = "Pool")
dom.df <- 
  dom.df %>%
  mutate(thresh = as.character(thresh))

# plot the change in species richness as a histogram
p1 <- 
  dom.df %>%
  filter(thresh == 0.05) %>%
  ggplot(data = .,
         mapping = aes(x = diff_SR)) +
  geom_histogram(alpha = 0.4) +
  ylab("Frequency") +
  xlab("Species richness change") +
  ggtitle(label = "") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", size = 0.8) +
  theme_meta() +
  theme(title = element_text(size = 30))
p1

dom.df1 <- 
  dom.df %>% 
  filter(diff_SR < 0) %>%
  mutate(diff_SR = abs(diff_SR))

dom.df1 %>%
  filter(thresh == 0.05) %>%
  nrow()

# plot the change in species richness and the extinction of species at different thresholds
p2 <- 
  ggplot(data = dom.df1,
       mapping = aes(x = diff_SR, y = n_ext, colour = thresh)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.75, size = 2, shape = 16) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_colour_viridis_d(option = "C", end = 0.95) +
  #scale_fill_viridis_d(option = "C", end = 0.95) +
  scale_x_continuous(limits = c(-0.2, max(dom.df1$diff_SR)+0.2 ), breaks = seq(0, 10, 2)) +
  scale_y_continuous(limits = c(-0.2, max(dom.df1$diff_SR)+0.2),  breaks = seq(0, 10, 2)) +
  ylab("Number of extinctions (1993-2016)") +
  xlab("Species richness decrease (1993-2016)") +
  labs(colour = "Extinction threshold (>)") +
  theme_meta() +
  theme(legend.position = "top",
        legend.key = element_rect(fill = NA, color = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.1, 'cm'))
p2

# plot histograms of the relative abundance of species going extinct and colonising
ext_col.df <- 
  rbind(data.frame(ext_col = "Extinctions",
                 ra = unlist(lapply(dom, function(x) x[[2]] ), use.names = FALSE)),
        data.frame(ext_col = "Colonisations",
                 ra = unlist(lapply(dom, function(x) x[[3]] ), use.names = FALSE)) )

# plot these data
p3 <- 
  ggplot(data = ext_col.df,
       mapping = aes(x = ra, colour = ext_col, fill = ext_col)) +
  geom_density() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_colour_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  facet_wrap(~ext_col, scales = "free_y") +
  xlab("Relative abundance (1993)") +
  ylab("Density") +
  theme_meta() +
  theme(legend.position = "none")

library(ggpubr)

ggarrange( ggarrange(p1, p2), p3,
           ncol = 1, nrow = 2)

### END
