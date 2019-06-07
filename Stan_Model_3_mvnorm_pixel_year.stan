// Stan Model

  // Season minumum b1
  // Season Amplitude b2 
  // green-up rate b3
  // SoS b4
  // Summer greeen-down b5

data {
  
  int N; // number of observations
  int NY; // number of years
  int NP; // number of pixels
  real y[N]; // response (i.e. observations)
  real doy[N]; // doy (day of year) is the predictor variable for the VI
  // year of observation; will be used as hierarchical level
  int<lower=0, upper=NY> year[N]; 
  int<lower=0, upper=NP> pixel[N]; 
  vector[5] beta_mean_scale; // mean scale of betas (especially for SoS)
  vector[5] sigma_scaling_beta; // sd of betas (sigmas)
}

parameters {

  // five parameters (b1-b5) are correlated -> matrix 5*5 diagnonal = covar for mvnorm
  corr_matrix[5] cor_matrix_beta;
  
  real<lower=0> sigma; //standard deviation of the Likelihood (truncated at zero)
  vector[NY] phi; // hierarchical level to allow for SoS variation
  real<lower=0> sigma_phi; // variation of phi
  vector[NP] pixel_phi;
  real<lower=0> sigma_pixel_phi;
  

  vector<lower=0>[5] sigma_beta_raw; // to use the scaling parameters
  
  vector<lower=0>[5] mean_beta_raw;
  vector[5] beta;
  
  
  
}

transformed parameters {
  // scaled mean for the mvnorm
  vector[5] beta_mean;
  vector<lower=0>[5] sigma_beta; // sigma for the beta matrix
  cov_matrix[5] cov_matrix_beta;

    for(i in 1:5)
      beta_mean[i] = mean_beta_raw[i] * beta_mean_scale[i];



 
  
  // use the scaling vector
    for(i in 1:5)
      sigma_beta[i] = sigma_beta_raw[i] * sigma_scaling_beta[i];
  
    cov_matrix_beta = quad_form_diag(cor_matrix_beta, sigma_beta);
 // scaled sigma for the betas

  
  
}

 

model {  
  vector[N] mu;
  // Priors
  sigma ~ normal(0, 10); // prior for sigma

  sigma_beta_raw ~ normal(0, 1);
  mean_beta_raw ~ normal(0, 1);
  
  cor_matrix_beta ~ lkj_corr(2); // LKJ prior on the correlation matrix 
  
  beta ~ multi_normal(beta_mean, cov_matrix_beta);
    
  
  
  phi ~ normal(0, sigma_phi); // prior for phi; to allow for intra-annual variation in SoS
  sigma_phi ~ cauchy(0, 5); // prior for phis sd
  
  pixel_phi ~ normal(0, sigma_pixel_phi);
  sigma_pixel_phi ~ cauchy(0, 5);
  
  // Likelihood
   for(i in 1:N)
    mu[i] = (beta[1]) + ((beta[2]) - (beta[5]) * doy[i]) * 
        (1 / (1 + exp(-(beta[3]) * (doy[i] - (beta[4] + phi[year[i]] + pixel_phi[pixel[i]])))));
  
  y ~ normal(mu, sigma);

}
