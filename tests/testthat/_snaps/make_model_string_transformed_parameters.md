# make_model_string_transf_param works with exponential survival and full borrowing

    Code
      result
    Output
      transformed parameters {
        real HR_trt = exp(beta_trt);
      }

# make_model_string_transf_param works with exponential survival and BDB

    Code
      result
    Output
      transformed parameters {
        real HR_trt = exp(beta_trt);
      }

# make_model_string_transf_param works with weibull survival and BDB and covariates

    Code
      result
    Output
      transformed parameters {
        real HR_trt = exp(beta_trt);
      }

# make_model_string_transf_param works with binary outcome and BDB  and covariates

    Code
      result
    Output
      transformed parameters {
        real OR_trt = exp(beta_trt);
      }

