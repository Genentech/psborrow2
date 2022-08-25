# make_model_string_data works with exponential survival and full borrowing

    Code
      result
    Output
      data {
      int<lower=0> N;
      vector[N] time;
      vector[N] cens;
      vector[N] trt; }

# make_model_string_data works with exponential survival and BDB

    Code
      result
    Output
      data {
      int<lower=0> N;
      vector[N] time;
      vector[N] cens;
      vector[N] trt;
      matrix[N,2] Z; }

# make_model_string_data works with weibull survival and BDB and covariates

    Code
      result
    Output
      data {
      int<lower=0> N;
      vector[N] time;
      vector[N] cens;
      vector[N] trt;
      matrix[N,2] Z;
      int<lower=0> K;
      matrix[N, K] X; }

# make_model_string_data works with binary outcome and BDB and covariates

    Code
      result
    Output
      data {
      int<lower=0> N;
      array[N] int y;
      vector[N] trt;
      matrix[N,2] Z;
      int<lower=0> K;
      matrix[N, K] X; }

