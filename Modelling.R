#############################################################################
#                           Maximilian Wesemeyer                            #
# Modelling phenology using all available Landsat Data for a pixel          #
#############################################################################


# installing the package from the GitHub repo
#if(!require(devtools)) install.packages('devtools')
#devtools::install_github('bnasr/phenocamapi')
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(loo)
library(rstan)

# Set rstan settings

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

# Data list
data <- list(N = length(data_stan_sub_1_level$vi), y =  data_stan_sub_1_level$vi, 
             doy = data_stan_sub_1_level$doy, 
             year = ((data_stan_sub_1_level$year-min(data_stan_sub_1_level$year)+1)), 
             NY = as.integer(length(unique(data_stan_sub_1_level$year))))
# fit the model with one hierarchical level
fit_pheno <- stan("Stan_Model_1_level_year.stan", data = data, chains = 2, iter = 2000)
fit_pheno
mcmc_trace(as.array(fit_pheno), pars = c("beta1", "beta2", "beta3", 
                                         "beta4", "beta5", "sigma"))

#stan_gam()
  # Dan Simpson understanding hircharchical gams

# fit the model with two hierarchical level
# Data list
scaling_vector_mean <- c(0,0,0,10,5)

data <- list(N = length(data_stan_sub_2_level$vi), y =  data_stan_sub_2_level$vi, 
             doy = data_stan_sub_2_level$doy, 
             year = ((data_stan_sub_2_level$year-min(data_stan_sub_2_level$year)+1)), 
             NY = as.integer(length(unique(data_stan_sub_2_level$year))),
             NP = length(data_stan_sub_2_level$pixel), 
             pixel = ((data_stan_sub_2_level$pixel-min(data_stan_sub_2_level$pixel)+1)),
             scaling_mean = scaling_vector_mean)


fit_pheno <- stan("Stan_Model_2_level_year_pixel.stan", data = data, chains = 1, iter = 1000)
fit_pheno
mcmc_trace(as.array(fit_pheno), pars = c("beta1", "beta2", "beta3", 
                                         "beta4", "beta5", "sigma"))

