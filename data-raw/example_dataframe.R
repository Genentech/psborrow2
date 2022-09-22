library(simsurv)

true_hr <- 0.6
drift_hr <- 1.0
cov1 <- 0.52
cov2 <- 1.1
cov3 <- 0.71

n <- 600

set.seed(20220921)
# Create a data frame with the subject IDs and treatment covariate
cov <- data.frame(
  id = 1:n,
  cov1 = rbinom(n, 1, 0.5),
  trt = 0
)
cov$trt[sample(600, 200)] <- 1
cov$ext <- ifelse(cov$trt == 1L, 0L, rbinom(sum(cov$trt), 1, 0.5))
cov$cov2 <- ifelse(cov$trt == 1L, rpois(n, 5), rpois(n, 7))
cov$cov3 <- ifelse(cov$ext == 1L, rnorm(n, cov$cov2, sd = 4), rnorm(n, cov$cov2, sd = 3))

# Simulate the event times
dat <- simsurv(
  lambdas = 0.1,
  gammas = 1.5,
  betas = c(
    trt = log(true_hr),
    ext = log(drift_hr),
    cov1 = log(cov1),
    cov2 = log(cov2),
    cov3 = log(cov3)
  ),
  x = cov,
  maxt = 24
)

dat$censor <- 1 - dat$status

# Merge the simulated event times onto covariate data frame
example_surv <- merge(cov, dat)[, -1]
example_surv <- example_surv[c("trt", "ext", "eventtime", "status", "censor", "cov1", "cov2", "cov3")]

usethis::use_data(example_surv, overwrite = TRUE)
