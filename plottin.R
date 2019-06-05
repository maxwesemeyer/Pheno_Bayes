#############################################################################
#                           Maximilian Wesemeyer                            #
# plotting the results from the Stan model                                  #
#############################################################################
fit_pheno_df <- as.data.frame(fit_pheno)
beta1 <- mean(fit_pheno_df$beta1)
beta2 <- mean(fit_pheno_df$beta2)
beta3 <- mean(fit_pheno_df$beta3)
beta4 <- mean(fit_pheno_df$beta4)
beta5 <- mean(fit_pheno_df$beta5)


seq_doy <- unique(order(data_stan_sub_1_level$doy))
vector_y <- c()
for(i in seq_doy) {
y_value <- (beta1) + ((beta2) - (beta5) * i) * 
  (1 / (1 + exp(-(beta3) * (i - (beta4)))))
vector_y <- c(vector_y, y_value)
}
results_df <- data.frame(x = seq_doy,y = vector_y)


ggplot(results_df, aes(x = x, y = y)) + geom_line(aes(col="red", size = 0.5)) +
  geom_point(aes(data_stan_sub_1_level$doy, data_stan_sub_1_level$vi))

#############################################################################
# extract the hierarchical level
#############################################################################
phi <- fit_pheno_df[,7:43]
fit_pheno_df[,7:ncol(fit_pheno_df)]


phi_df <- data.frame(year = sort(unique(data_stan_sub_1_level$year)), 
                     phi = phi %>% apply(2, mean),
                     phi %>% apply(2, function(x) quantile(x, probs= c(0.025, 0.25, 0.5, 0.75, 0.975))) %>% t())

ggplot(phi_df, aes(x = year, y = phi)) + geom_point() +
  geom_hline(yintercept = 0, col = "red") + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.))


#############################################################################
# extract the hierarchical level; 2 levels
#############################################################################

phi <- fit_pheno_df[,7:ncol(fit_pheno_df)]


phi_df <- data.frame(phi = phi %>% apply(2, mean),
                     phi %>% apply(2, function(x) quantile(x, probs= c(0.025, 0.25, 0.5, 0.75, 0.975))) %>% t())

ggplot(phi_df, aes(x = year, y = phi)) + geom_point() +
  geom_hline(yintercept = 0, col = "red") + 
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.))

