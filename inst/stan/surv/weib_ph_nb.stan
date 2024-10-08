// Weibull proportional hazards survival model
// No / full borrowing

functions {
  real weibull_ph_lpdf(real y, real alpha, real lambda) {
    real lprob = log(alpha) + log(lambda) + (alpha - 1) * log(y) - lambda * (y^alpha);
    return lprob;
  }

  real weibull_ph_lcdf(real y, real alpha, real lambda) {
    real lprob = log(1 - exp(-lambda * y^alpha));
    return lprob;
  }

  real weibull_ph_lccdf(real y, real alpha, real lambda) {
    real lprob = -lambda * y^alpha;
    return lprob;
  }
}

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
  real shape_weibull;   // weibull shape parameter
  
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
  {{ shape.prior }}
  
  lp = alpha + trt * beta_trt {{ cov.linpred }} ;
  elp = exp(lp);
  
  for (i in 1:N) {
    if (cens[i] == 1) {
      target += weibull_ph_lccdf(time[i] | shape_weibull, elp[i]) {{ weights.likelihood }};
    } else {
      target += weibull_ph_lpdf(time[i] | shape_weibull, elp[i]) {{ weights.likelihood }};
    }
  }
}
