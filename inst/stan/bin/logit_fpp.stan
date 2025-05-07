// Logistic regression
// Fixed power prior

data {

  int<lower=0> N;      // number of observations
  vector[N] trt;       // treatment indicator
  array[N] int y;      // outcome
  vector<lower=0, upper=1>[N] power;     // power parameter

  {{ weights.data }}
  {{ cov.data }}

}

parameters {

  real beta_trt;     // treatment effect
  real alpha;        // baseline log-odds

  {{ cov.parameters }}

}

transformed parameters {

  real OR_trt = exp(beta_trt);

}

model {

  vector[N] lp;

  {{ trt.prior }}
  {{ cov.priors }}
  {{ baseline.prior }}

  lp = alpha + trt * beta_trt {{ cov.linpred }} ;

  for (i in 1:N) {
    target += bernoulli_logit_lupmf(y[i] | lp[i]) * power[i] {{ weights.likelihood }};
  }

}
