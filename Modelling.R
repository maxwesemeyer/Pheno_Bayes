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
scaling_vector_mean <- c(0.45, 2.4 , 0.15 , 118, 0.0005)
sigma_scaling <- c(0.1, 0.1, 0.1, 0.1, 0.1)

data <- list(N = length(data_stan_sub_2_level$vi), 
             y =  data_stan_sub_2_level$vi, 
             doy = data_stan_sub_2_level$doy, 
             year = ((data_stan_sub_2_level$year-min(data_stan_sub_2_level$year)+1)), 
             NY = as.integer(length(unique(data_stan_sub_2_level$year))),
             NP = length(data_stan_sub_2_level$pixel), 
             pixel = ((data_stan_sub_2_level$pixel-min(data_stan_sub_2_level$pixel)+1)),
             beta_mean_scale = scaling_vector_mean,
             sigma_scaling_beta = sigma_scaling)


fit_pheno <- stan("Stan_Model_2_level_year_pixel.stan", data = data, chains = 1, iter = 1000)
fit_pheno
mcmc_trace(as.array(fit_pheno), pars = c("beta1", "beta2", "beta3", 
                                         "beta4", "beta5", "sigma"))


#############################################################################
###################### don't use
#############################################################################
fit_arm <- stan_glmer(vi ~ doy + pixel + (1 | year), data = data_stan_sub_2_level )
hist(bayesplot::rhat(fit_arm))
as.data.frame(fit_arm)

newdat <- data.frame(doy = seq_doy, pixel = c(rep(1, 100), rep(2, 141)))
post_pred <- posterior_linpred(fit_arm, newdata = newdat, re.form = NA)
post_pred <- apply(post_pred, 2, mean)
ggplot(newdat, aes(x = newdat$doy, y = post_pred)) + geom_point()



#############################################################################
# third model
#############################################################################

# Data list

data_stan_sub_3_level$year_recode <- ((data_stan_sub_3_level$year-min(data_stan_sub_3_level$year)+1))
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 3),"pixel_recode"] <- 1
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 10),"pixel_recode"] <- 2
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 11),"pixel_recode"] <- 3
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 13),"pixel_recode"] <- 4


data_stan_sub_3_level$pixel_recode
unique(data_stan_sub_3_level$pixel_recode)
scaling_vector_mean <- c(0.45, 2.4 , 0.15 , 118, 0.0005)
sigma_scaling <- c(0.1, 1, 0.1, 10, 0.1)

data <- list(N = length(data_stan_sub_3_level$vi), 
             y =  data_stan_sub_3_level$vi, 
             doy = data_stan_sub_3_level$doy, 
             year = data_stan_sub_3_level$year_recode, 
             NY = as.integer(length(unique(data_stan_sub_3_level$year))),
             NP = length(unique(data_stan_sub_3_level$pixel)), 
             pixel = data_stan_sub_3_level$pixel_recode,
             beta_mean_scale = scaling_vector_mean,
             sigma_scaling_beta = sigma_scaling)


fit_pheno <- stan("Stan_Model_3_mvnorm_pixel_year.stan", data = data, chains = 1, iter = 1000)
fit_pheno$`cor_matrix_beta[4,1]`
mcmc_trace(as.array(fit_pheno), pars = c("sigma"))


#############################################################################
# fourth model
#############################################################################

# Data list

data_stan_sub_3_level$year_recode <- ((data_stan_sub_3_level$year-min(data_stan_sub_3_level$year)+1))
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 3),"pixel_recode"] <- 1
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 10),"pixel_recode"] <- 2
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 11),"pixel_recode"] <- 3
data_stan_sub_3_level[which(data_stan_sub_3_level$pixel == 13),"pixel_recode"] <- 4


data_stan_sub_3_level$pixel_recode
unique(data_stan_sub_3_level$pixel_recode)
scaling_vector_mean <- c(0.45, 2.4 , 0.15 , 118, 0.0005)
sigma_scaling <- c(0.1, 1, 0.1, 10, 0.1)

data <- list(N = length(data_stan_sub_3_level$vi), 
             y =  data_stan_sub_3_level$vi, 
             doy = data_stan_sub_3_level$doy, 
             year = data_stan_sub_3_level$year_recode, 
             NY = as.integer(length(unique(data_stan_sub_3_level$year))),
             NP = length(unique(data_stan_sub_3_level$pixel)), 
             pixel = data_stan_sub_3_level$pixel_recode,
             beta_mean_scale = scaling_vector_mean,
             beta_sigma_scale = sigma_scaling)


fit_pheno <- stan("Stan_Model_4_+spatialvar.stan", data = data, chains = 2, iter = 1000)

mcmc_trace(as.array(fit_pheno), pars = c("sigma"))


