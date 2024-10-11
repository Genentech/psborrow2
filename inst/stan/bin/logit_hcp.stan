// Logistic regression
// Hierarchical commensurate prior

data {

  int<lower=0> N;      // number of observations
  vector[N] trt;       // treatment indicator
  array[N] int y;      // outcome
  matrix[N,2] Z;       // external flag indicators

  {{ weights.data }}
  {{ cov.data }}

}

parameters {

  real beta_trt;     // treatment effect
  vector[2] alpha;   // baseline log-odds
  real<lower=0> tau; // precision on dynamic borrowing
  
  {{ cov.parameters }}

}

transformed parameters {

  real OR_trt = exp(beta_trt);

}

model {

  vector[N] lp;
  real sigma;

  {{ trt.prior }}
  {{ cov.priors }}
  {{ baseline.prior }}
  {{ tau.prior }}

  sigma = 1 / tau;
  alpha[1] ~ normal(alpha[2], sqrt(sigma));
  
  lp = Z * alpha + trt * beta_trt {{ cov.linpred }} ;
  
  for (i in 1:N) {
    target += bernoulli_logit_lupmf(y[i] | lp[i]) {{ weights.likelihood }};
  }
  
}
