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


seq_doy <- unique(order(data_stan_sub$doy))
vector_y <- c()
for(i in seq_doy) {
y_value <- (beta1) + ((beta2) - (beta5) * i) * 
  (1 / (1 + exp(-(beta3) * (i - (beta4)))))
vector_y <- c(vector_y, y_value)
}
results_df <- data.frame(x = seq_doy,y = vector_y)


ggplot(results_df, aes(x = x, y = y)) + geom_line(aes(col="red", size = 0.5)) +
  geom_point(aes(data_stan_sub$doy, data_stan_sub$vi))

