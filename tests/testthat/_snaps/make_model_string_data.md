# make_model_string_data works with exponential survival and full borrowing

    Code
      result
    Output
      data {
        int<lower=0> N;
        vector[N] trt;
        vector[N] time;
      vector[N] cens;
      
        
        
      }

# make_model_string_data works with exponential survival and BDB

    Code
      result
    Output
      data {
        int<lower=0> N;
        vector[N] trt;
        vector[N] time;
      vector[N] cens;
      
        matrix[N,2] Z;
        
      }

# make_model_string_data works with weibull survival and BDB and covariates

    Code
      result
    Output
      data {
        int<lower=0> N;
        vector[N] trt;
        vector[N] time;
      vector[N] cens;
      
        matrix[N,2] Z;
        int<lower=0> K;
      matrix[N, K] X;
      vector[K] L_beta;
      vector[K] U_beta;
      }

# make_model_string_data works with binary outcome and BDB and covariates

    Code
      result
    Output
      data {
        int<lower=0> N;
        vector[N] trt;
        array[N] int y;
      
        matrix[N,2] Z;
        int<lower=0> K;
      matrix[N, K] X;
      vector[K] L_beta;
      vector[K] U_beta;
      }

# make_model_string_data works with binary outcome and weights

    Code
      result
    Output
      data {
        int<lower=0> N;
        vector[N] trt;
        array[N] int y;
      vector[N] weight;
        matrix[N,2] Z;
        int<lower=0> K;
      matrix[N, K] X;
      vector[K] L_beta;
      vector[K] U_beta;
      }

