// Stan Model

data {
  
  int N; // number of observations
  real y[N]; // response (i.e. observations)
  real doy[N]; // doy (day of year) is the predictor variable for the VI

}

parameters {


  real<lower=0> sigma; //standard deviation (truncated at zero)
  real beta1; // Season minumum
  real beta2; // Season Amplitude
  real<lower=0> beta3; //  green-up rate
  real<lower=0> beta4; // SoS
  real beta5; // Summer greeen-down
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
  
  // Likelihood
   for(i in 1:N)
    mu[i] = (beta1) + ((beta2) - (beta5) * doy[i]) * 
        (1 / (1 + exp(-(beta3) * (doy[i] - (beta4)))));
  
  y ~ normal(mu, sigma);
  

}
