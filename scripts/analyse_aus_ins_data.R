
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

# plot species richness by cluster size
ggplot(data = env,
       mapping = aes(x = cluster_size, y = SR)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  theme_meta()


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















