
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

# load the plotting theme
source(here("scripts/Function_plotting_theme.R"))

# load the community data
com <- read_csv(here("data/analysis_data/aus_ins_com.csv"))
head(com)

# load the environmental data
env <- read_csv(here("data/analysis_data/aus_ins_env.csv"))
head(env)
summary(env)

# does the community data and the environmental data have the same sites?
all(sort(unique(com$site)) == sort(env$site)) 

# calculate relative abundance
com <- 
  com %>%
  group_by(site) %>%
  mutate(sum_abundance = sum(abundance)) %>%
  ungroup() %>%
  mutate(relative_abundance = round( abundance/sum_abundance, 5) ) %>%
  select(-sum_abundance)
View(com)

# calculate species richness
env <- 
  full_join(env, 
            com %>%
              group_by(site) %>%
              summarise(SR = sum(ifelse(relative_abundance > 0, 1, 0)) ),
          by = "site")
View(env)

# fit a power function between species richness and cluster size
glm.x <- nls(SR ~ a*cluster_size^b, data = env)

# get predictions
# https://stackoverflow.com/questions/52973626/how-to-calculate-95-prediction-interval-from-nls
# confidence intervals calculated usin delta method
df.pred <- data.frame(cluster_size = seq(12, 410, 1))
glm.pred <- investr::predFit(glm.x, df.pred, interval="prediction")
df.pred <- cbind(df.pred, glm.pred)
names(df.pred) <- c("cluster_size", "SR", "lwr", "upr")

# plot species richness by cluster size
p1 <- 
  ggplot() +
  geom_ribbon(data = df.pred,
              mapping = aes(x = cluster_size, ymin = lwr, ymax = upr),
              alpha = 0.1) +
  geom_point(data = env,
             mapping = aes(x = cluster_size, y = SR)) +
  geom_line(data = df.pred,
            mapping = aes(x = cluster_size, y = SR)) +
  xlab("Number of pools (cluster size)") +
  ylab("Local species pool richness") +
  theme_meta()
plot(p1)

# what about environmental variation?
names(env)

# subset a set of environmental variables for the PCA
# minTP is the same across all mountains
env.pca <- env[, c("Avgdistothermountains", "Elevation", "MeanTotalNmgl",
                   "CVtotN", "MeanTotalPmgl", "CVtotP", "MinTN", "MaxTN",
                   "RangeTN", "MaxTP", "RangeTP", "Granitesurfacem2",
                   "Circumference", "rock_10km", "Saltlake", "Height_rel",
                   "height_ggearth", "Dist_to_paleodrainage")]
ncol(env.pca)

# which variables contain NAs
any(unlist(lapply(env.pca, function(x) sum(is.na(x)) )) > 0) # none have NAs

# standardise the variables
env.pca <- as_tibble(apply(env.pca, 2, function(x) (x - mean(x))/sd(x) ))
head(env.pca)

# run a PCA
pc1 <- prcomp(formula(paste("~ ", paste(names(env.pca), collapse = " + "), sep = "")),
              data = env.pca, center = FALSE, scale. = FALSE)
summary(pc1)

# check the biplot
biplot(pc1)


# is cluster size significantly correlated with any of the first five PC axes?
pc1.x <- as_tibble(pc1$x)

pvals1 <- vector()
for (i in 1:5) {
  y <- cor.test(env$cluster_size, pc1.x[[i]])
  print(y)
  
  # write the p-values into a vector
  pvals1[i] <- y$p.value
}

# correct the p-values
p.adjust(p = pvals1, method = "bonferroni")


# does heterogeneity vary with cluster size? We have four heterogeneity variables
pvals2 <- vector()
for (i in 1:4) {
  y <- cor.test(env$cluster_size, env[, c( "CVtotN", "CVtotP", "RangeTN", "RangeTP")][[i]])
  print(y)
  
  # write the p-values into a vector
  pvals2[i] <- y$p.value
}

# correct the p-values
p.adjust(p = pvals2, method = "bonferroni")

# arrange the environmental variables by site
env <- 
  env %>%
  arrange(site)


# match up the biomass data with the community data
bio_dat <- read_csv(here("data/biomass_conversions/aus_ins_bio.csv"))
head(bio_dat)
View(bio_dat)

# subset the relevant columns
bio_dat <- 
  bio_dat %>%
  select(species, life_stage, biomass_mg)

# join this to the community data
com_bio <- full_join(com, bio_dat, by = "species")
View(com_bio)

# calculate biomass
com_bio <- 
  com_bio %>%
  mutate(biomass_mg = abundance*biomass_mg)

# exclude sites where biomass is NA and species have relative abundance greater than 0.05
length(unique(com$site))

com_bio <- 
  com_bio %>%
  group_by(site) %>%
  mutate(exclude = ifelse(any(is.na(biomass_mg) & (relative_abundance > 0.05)), 1, 0)) %>%
  filter(exclude == 0)
length(unique(com_bio$site))

# multiply the abundance by each biomass value
env$biomass_mg <- 
  com_bio %>%
  group_by(site) %>%
  summarise(biomass_mg = sum(biomass_mg, na.rm = TRUE), .groups = "drop") %>%
  arrange(site) %>%
  pull(biomass_mg)

# plot species richness by cluster size
p2 <- 
  ggplot(data = env,
       mapping = aes(x = SR, y = biomass_mg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, colour = "black", alpha = 0.2) +
  xlab("Local species pool richness") +
  ylab("Biomass (mg)") +
  theme_meta()
plot(p2)

lm.x <- lm(biomass_mg ~ SR, data = env)
plot(lm.x)
summary(lm.x)

# check that we have a figures folder
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# arrange these two plots
p12 <- ggpubr::ggarrange(p1, p2, labels = c("a", "b"),
                         font.label = list(size = 11, color = "black", face = "plain", family = NULL))
ggsave(filename = here("figures/fig_4.png"), p12,
       width = 18, height = 9, units = "cm", dpi = 300)

### END
