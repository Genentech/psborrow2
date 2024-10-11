// Continuous normal
// No / full borrowing

data {

  int<lower=0> N;                // number of observations
  vector[N] trt;                 // treatment indicator
  array[N] real y;               // outcome

  {{ weights.data }}
  {{ cov.data }}

}

parameters {

  real beta_trt;                 // treatment effect
  real<lower=0> std_dev_outcome; // standard deviation of outcome
  real alpha;                    // baseline mean

  {{ cov.parameters }}

}

model {
  
  vector[N] lp;
  
  {{ trt.prior }}
  {{ cov.priors }}
  {{ baseline.prior }}
  {{ stdev.prior }}

  sigma = 1 / tau;
  alpha[1] ~ normal(alpha[2], sqrt(sigma));

  lp = Z * alpha + trt * beta_trt {{ cov.linpred }} ;
  
  for (i in 1:N) {
    target += normal_lupdf(y[i] | lp[i], std_dev_outcome) {{ weights.likelihood }};
  }

}
