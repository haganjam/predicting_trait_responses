# predicting_trait_responses
Project using a long-term dataset to predict changes in trait distributions through time

> hydrological_model_function.R

This script is an implementation of the hydrological model described in Vanschoenwinkel et al. (2009) and Tuytens et al. (2014) to reconstruct the water levels of rock pools on inselbergs.

> parameterise_the_C_term.R

One of the model terms is a parameter that describes how daily evaporation measured from a nearby weather station is related to evaporation in the individual rock pools. In previous studies (e.g. those cited above), the C-term has been estimated using mean monthly evaporation data and then run using daily precipitation data and daily evaporation data.

Here, I estimate the C-term using daily evaporation data and find almost no difference.

> reconstruct_pool_hydrology.R

This script reconstructs the pool depth of 36 different rock pools using daily rainfall data and daily precipitation data from 1980 until 2019.

In addition, we test this model on a set of empirically measured rock pool water levels and find reasonable correlations (i.e. mean among rock pools of 0.56). Thus, at least in terms of qualitative differences through time, the model appears to perform well.
