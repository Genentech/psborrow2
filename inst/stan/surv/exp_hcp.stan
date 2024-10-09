// Exponential survival model
// Hierarchical commensurate prior

data {

  int<lower=0> N;       // number of observations
  vector[N] trt;        // treatment indicator
  vector[N] time;       // survival time
  vector[N] cens;       // censoring indicator
  matrix[N,2] Z;        // external flag indicators

  {{ weights.data }}
  {{ cov.data }}

}

parameters {

  real beta_trt;        // treatment effect                                
  vector[2] alpha;      // baseline hazard
  real<lower=0> tau;    // precision on dynamic borrowing
  
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
  {{ tau.prior }}
  
  sigma = 1 / tau;
  alpha[1] ~ normal(alpha[2], sqrt(sigma));  

  lp = Z * alpha + trt * beta_trt {{ cov.linpred }} ;
  elp = exp(lp);
  
  for (i in 1:N) {
    if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i]) {{ weights.likelihood }};
    } else {
      target += exponential_lpdf(time[i] | elp[i]) {{ weights.likelihood }};
    }
  }
  
}
