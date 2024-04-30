functions {
  vector make_durations(vector starts, vector time) {
  vector [rows(starts)] ends = append_row(tail(starts, rows(starts) - 1), max(time));
  return fdim(ends, starts);
}
}

data {
  int<lower=0> N;
  vector[N] trt;
  vector[N] time;
vector[N] cens;

  
  int<lower=0> K;
matrix[N, K] X;
vector[K] L_beta;
vector[K] U_beta;
}

transformed data {
  int<lower = 1> M = 5;
vector[M] starts = [0, 10, 20, 30, 40];
vector[M] durations;
matrix[N,M] T;
matrix[N,M] D;

durations = make_durations(starts, time);

for(j in 1:M) {
  T[,j] = fmin(fdim(time, starts[j]), durations[j]);
}
for(j in 1:M) {
  for(i in 1:N) {
    D[i,j] = (starts[j] <= time[i] && time[i] < starts[j] + durations[j]) * (1 - cens[i]);
  }
}
}

  parameters {
real beta_trt;
vector[M] alpha;


vector<lower=L_beta, upper=U_beta>[K] beta;
}

  transformed parameters {
  real HR_trt = exp(beta_trt);
}

  model {
  matrix[N,M] lp;
  beta_trt ~ normal(0, 1000);
  lp = rep_matrix(alpha', N) + rep_matrix(trt * beta_trt + X * beta, M);
  
  beta[1] ~ normal(0, 1000) ;
beta[2] ~ normal(0, 1000) ;
  alpha ~ normal(0, 1000);
  target += sum((lp .* D) - (exp(lp) .* T));
}