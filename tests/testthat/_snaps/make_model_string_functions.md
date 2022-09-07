# make_model_string_functions works with exponential survival and full borrowing

    Code
      result
    Output
      functions {
        
      }

# make_model_string_functions works with exponential survival and BDB

    Code
      result
    Output
      functions {
        
      }

# make_model_string_functions works with weibull survival and BDB and covariates

    Code
      result
    Output
      functions {
         real weibull_ph_lpdf(real y, real alpha, real lambda) {
           real lprob = log(alpha) + log(lambda) + (alpha - 1) * log(y) - lambda * (y^alpha);
           return lprob;
       }
      
       real weibull_ph_lcdf(real y, real alpha, real lambda) {
           real lprob = log(1-exp(-lambda * y^alpha));
           return lprob;
       }
      
       real weibull_ph_lccdf(real y, real alpha, real lambda) {
           real lprob = -lambda * y^alpha;
           return lprob;
       }
      }

# make_model_string_functions works with binary outcome and BDB and covariates

    Code
      result
    Output
      functions {
        
      }

