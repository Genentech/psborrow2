# make_model_string_model works with exponential survival and full borrowing

    Code
      result
    Output
      model {
        vector[N] lp;
        vector[N] elp;
        beta_trt ~ normal(0, 1000);
        lp = alpha + trt * beta_trt ;
      elp = exp(lp);
        
        
        alpha ~ normal(0, 100) ;
        for (i in 1:N) {
         if (cens[i] == 1) {
            target += exponential_lccdf(time[i] | elp[i] );
         } else {
            target += exponential_lpdf(time[i] | elp[i] );
         }
      }
      }

# make_model_string_model works with exponential survival and BDB

    Code
      result
    Output
      model {
        vector[N] lp;
        vector[N] elp;
        beta_trt ~ normal(0, 1000);
        lp = Z * alpha + trt * beta_trt;
      elp = exp(lp) ;
        
        
        tau ~ exponential(0.001) ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ normal(0, 100) ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;
        for (i in 1:N) {
         if (cens[i] == 1) {
            target += exponential_lccdf(time[i] | elp[i] );
         } else {
            target += exponential_lpdf(time[i] | elp[i] );
         }
      }
      }

# make_model_string_model works with weibull survival and BDB and covariates

    Code
      result
    Output
      model {
        vector[N] lp;
        vector[N] elp;
        beta_trt ~ normal(0, 1000);
        lp = X * beta + Z * alpha + trt * beta_trt;
      elp = exp(lp) ;
        shape_weibull ~ normal(0, 1000) ;
        beta[1] ~ normal(0, 1000) ;
      beta[2] ~ normal(0, 1000) ;
        tau ~ exponential(0.001) ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ normal(0, 100) ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;
        for (i in 1:N) {
         if (cens[i] == 1) {
            target += weibull_ph_lccdf(time[i] | shape_weibull, elp[i] );
         } else {
            target += weibull_ph_lpdf(time[i] | shape_weibull, elp[i] );
         }
      }
      }

# make_model_string_model works with binary outcome and BDB and covariates

    Code
      result
    Output
      model {
        vector[N] lp;
        vector[N] elp;
        beta_trt ~ normal(0, 1000);
        lp = X * beta + Z * alpha + trt * beta_trt;
      elp = exp(lp) ;
        
        beta[1] ~ normal(0, 1000) ;
      beta[2] ~ normal(0, 1000) ;
        tau ~ exponential(0.001) ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ normal(0, 100) ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;
        for (i in 1:N) {
        target += bernoulli_logit_lupmf(y[i] | lp[i]);
      }
      }

