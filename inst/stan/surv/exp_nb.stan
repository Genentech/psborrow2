// Exponential survival model
// No / full borrowing

data {

  int<lower=0> N;       // number of observations
  vector[N] trt;        // treatment indicator
  vector[N] time;       // survival time
  vector[N] cens;       // censoring indicator

  {{ weights.data }}
  {{ cov.data }}
}

parameters {
  real beta_trt;        // treatment effect                                
  real alpha;           // baseline hazard
  
  {{ cov.parameters }}
  
}

transformed parameters {
  real HR_trt = exp(beta_trt);
}

model {
  
  vector[N] lp;
  vector[N] elp;
  
  {{ trt.prior }}
  {{ cov.priors }}
  {{ baseline.prior }}
  
  lp = alpha + {{ cov.linpred }} + trt * beta_trt;
  elp = exp(lp);
  
  for (i in 1:N) {
    if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i]) {{ weights.likelihood }};
    } else {
      target += exponential_lpdf(time[i] | elp[i]) {{ weights.likelihood }};
    }
  }
}
