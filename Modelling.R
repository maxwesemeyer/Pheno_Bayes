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
data <- list(N = length(data_prep$vi), y =  data_prep$vi)

fit <- stan("Stan_Model.stan", data = data)
