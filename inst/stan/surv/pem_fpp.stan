// Piecewise exponential survival model
// Fixed power prior

data {

  int<lower=0> N;                 // number of observation-periods
  vector[N] trt;                  // treatment indicator
  vector[N] time;                 // survival time
  vector[N] cens;                 // censoring indicator
  int N_periods;                  // number of periods
  matrix[N, N_periods] Z;         // period indicators
  vector<lower=0, upper=1>[N] power;     // power parameter

  {{ weights.data }}
  {{ cov.data }}

}

parameters {

  real beta_trt;                // treatment effect
  vector[N_periods] alpha;      // baseline hazard

  {{ cov.parameters }}

}

transformed parameters {

  real HR_trt = exp(beta_trt);

}

model {

  vector[N] lp;
  vector[N] elp;
  real sigma;

  {{ trt.prior }}
  {{ cov.priors }}

  for (i in 1:N_periods) {
    alpha[i] ~ {{ baseline.prior }};
  }

  lp = Z * alpha + trt * beta_trt {{ cov.linpred }} ;
  elp = exp(lp);

  for (i in 1:N) {
    if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i]) * power[i] {{ weights.likelihood }};
    } else {
      target += exponential_lpdf(time[i] | elp[i]) * power[i] {{ weights.likelihood }};
    }
  }

}
