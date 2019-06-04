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

# Data list
data <- list(N = length(data_stan_sub$vi), y =  data_stan_sub$vi, doy = data_stan_sub$doy)

fit_pheno <- stan("Stan_Model.stan", data = data, chains = 2, iter = 2000)
fit_pheno
mcmc_trace(as.array(fit_pheno), pars = c("beta1", "beta2", "beta3", 
                                         "beta4", "beta5", "sigma"))

#stan_gam()
# Dan Simpson understanding hircharchical gams

