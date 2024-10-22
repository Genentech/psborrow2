// Piecewise exponential survival model
// Hierarchical commensurate prior

data {

  int<lower=0> N;       // number of observation-periods
  vector[N] trt;        // treatment indicator
  vector[N] time;       // survival time
  vector[N] cens;       // censoring indicator
  vector[N] Z0;         // period indicators - internal
  vector[N] Z1;         // period indicators - external
  int N_periods;        // number of periods

  {{ weights.data }}
  {{ cov.data }}

}

parameters {

  real beta_trt;                // treatment effect                                
  vector[N_periods] alpha0;     // baseline hazard - internal
  vector[N_periods] alpha1;     // baseline hazard - external
  real<lower=0> tau;            // precision on dynamic borrowing

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
  {{ baseline.prior }}

  sigma = 1 / tau;
  for (i in 1:N_periods) {
    alpha0[i] ~ normal(alpha1[i], sqrt(sigma));
  }

  lp = Z0 * alpha0 + Z1 * alpha1 + trt * beta_trt {{ cov.linpred }} ;
  elp = exp(lp);

  for (i in 1:N) {
    if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i]) {{ weights.likelihood }};
    } else {
      target += exponential_lpdf(time[i] | elp[i]) {{ weights.likelihood }};
    }
  }

}
