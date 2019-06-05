// Stan Model

data {
  
  int N; // number of observations
  int NY; // number of years
  int NP; // number of pixels
  real y[N]; // response (i.e. observations)
  real doy[N]; // doy (day of year) is the predictor variable for the VI
  // year of observation; will be used as hierarchical level
  int<lower=0, upper=NY> year[N]; 
  int<lower=0, upper=NP> pixel[N]; 
  vector[5] scaling_mean;
}

parameters {


  real<lower=0> sigma; //standard deviation of the Likelihood (truncated at zero)
  real beta1; // Season minumum
  real beta2; // Season Amplitude
  real<lower=0> beta3; //  green-up rate
  real<lower=0> beta4; // SoS
  real beta5; // Summer greeen-down
  vector[NY] phi; // hierarchical level to allow for SoS variation
  real<lower=0> sigma_phi; 
  vector[NP] pixel_phi_1; 
  vector[NP] pixel_phi_2; 
  vector[NP] pixel_phi_3; 
  vector[NP] pixel_phi_4; 
  vector[NP] pixel_phi_5; 
  // sigma for the pixel variations
  vector[5] pixel_phi;
  vector<lower=0>[5] pixel_phi_sig;
  matrix[5, NP] pixel_m;
  
}

transformed parameters {
  
  vector[5] pixel_phi_raw;
  // use the scaling vector
  for(i in 1:5)
    pixel_phi_raw[i] = pixel_phi_sig[i] * scaling_mean[i];
  
}

model {  
  vector[N] mu;
  // Priors
  sigma ~ normal(0, 10); // prior for sigma
  beta1 ~ normal(0.45,0.1);
  beta2 ~ normal(2.4,0.1);
  beta3 ~ normal(0.15,0.01);
  beta4 ~ normal(118,1);
  beta5 ~ normal(0.0005,0.01);
  for(i in 1:5)
    pixel_phi_sig[i] ~ normal(0,1);
    
  for(i in 1:5)
    pixel_phi[i] ~ normal(pixel_phi_raw[i], pixel_phi_sig[i]);
  
  
  phi ~ normal(0, sigma_phi); // prior for phi; to allow for intra-annual variation in SoS
  sigma_phi ~ cauchy(0, 5); // prior for phis sd
  // Likelihood
   for(i in 1:N)
    mu[i] = (beta1+pixel_phi[1,pixel[i]]) + ((beta2+pixel_phi[2, pixel[i]]) - (beta5+pixel_phi[5, pixel[i]]) * doy[i]) * 
        (1 / (1 + exp(-(beta3+pixel_phi[3, pixel[i]]) * (doy[i] - (beta4+pixel_phi[4, pixel[i]] + phi[year[i]])))));
  
  y ~ normal(mu, sigma);

}
