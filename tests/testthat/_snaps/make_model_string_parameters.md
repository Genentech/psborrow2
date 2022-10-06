# make_model_string_parameters works with exponential survival and full borrowing

    Code
      result
    Output
      parameters {
      real beta_trt;
      
      
      real alpha;
      
      }

# make_model_string_parameters works with exponential survival and BDB

    Code
      result
    Output
      parameters {
      real beta_trt;
      
      real<lower=0> tau;
      vector[2] alpha;
      
      }

# make_model_string_parameters works with weibull survival and BDB

    Code
      result
    Output
      parameters {
      real beta_trt;
      real shape_weibull;
      real<lower=0> tau;
      vector[2] alpha;
      vector<lower=L_beta, upper=U_beta>[K] beta;
      }

# make_model_string_parameters works with binary outcome and BDB

    Code
      result
    Output
      parameters {
      real beta_trt;
      
      real<lower=0> tau;
      vector[2] alpha;
      vector<lower=L_beta, upper=U_beta>[K] beta;
      }

