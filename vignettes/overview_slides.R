## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">"
)

## ----psborrow2, include = FALSE-----------------------------------------------
library(psborrow2)

## ----echo = FALSE-------------------------------------------------------------
library(gt)
types_of_borrowing <- data.frame(
  bt = c("No borrowing", "Full borrowing", "Bayesian dynamic borrowing (BDB)"),
  desc = c(
    "Only include the internal RCT date (i.e., ignoring external controls)",
    "Pool external and internal controls",
    "Borrowing external controls to the extent that the outcomes are similar"
  )
)
colnames(types_of_borrowing) <- c("Borrowing type", "Description")
tab_style(
  gt(types_of_borrowing),
  locations = cells_column_labels(columns = everything()),
  style = list(
    cell_text(weight = "bold")
  )
)

## ----echo = FALSE, fig.align="center", fig.dim = c(8,4)-----------------------
x_seq <- c(seq(0, 10, .001), seq(10, 10000, 1))
uninf_gamma <- vapply(x_seq, dgamma, shape = .001, rate = .001, FUN.VALUE = numeric(1))
inf_gamma <- vapply(x_seq, dgamma, shape = 1, rate = 0.001, FUN.VALUE = numeric(1))
gamma_dens_df <- data.frame(
  xval = c(x_seq, x_seq),
  dens = c(uninf_gamma, inf_gamma),
  type = c(
    rep("gamma(0.001, 0.001) -> Less borrowing", length(uninf_gamma)),
    rep("gamma(1, 0.001) -> More borrowing", length(inf_gamma))
  )
)
gamma_dens_df$type <- factor(gamma_dens_df$type, levels = c(
  "gamma(0.001, 0.001) -> Less borrowing",
  "gamma(1, 0.001) -> More borrowing"
))
library(ggplot2)
ggplot(gamma_dens_df) +
  geom_density(
    aes(
      x = xval,
      y = dens,
      color = type
    ),
    stat = "identity",
    size = 1.5
  ) +
  scale_x_continuous(limits = c(-.1, 5000)) +
  scale_y_continuous(limits = c(0, .01)) +
  labs(
    x = "tau",
    y = "density",
    color = "hyperprior"
  ) +
  theme(
    legend.position = "bottom"
  )

## ----echo = FALSE-------------------------------------------------------------
psborrow2_history <- data.frame(
  date = c("2019", "Q2 2021", "2021", "Q1 2022", "May 2022", "Jul 2022", "Oct 2022"),
  desc = c(
    "psborrow development started",
    "CRAN v0.1.0 published",
    "Initial user feedback received from the interial POC study and the FDA CID pilot project",
    "We start a collaboration with Roche statistical engineering team to productionize the package",
    "psborrow v0.2.0 published (bug fixed, documentation improved) on CRAN",
    "psborrow2 development started (faster, improved UI, tests, flexibility, more outcomes)",
    "psborrow2 package made public"
  )
)
colnames(psborrow2_history) <- c("Date", "Event")
tab_style(
  gt(psborrow2_history),
  locations = cells_column_labels(columns = everything()),
  style = list(
    cell_text(weight = "bold")
  )
)

## ----eval = FALSE-------------------------------------------------------------
#  bernoulli_prior()
#  beta_prior()
#  cauchy_prior()
#  exponential_prior()
#  gamma_prior()
#  normal_prior()
#  poisson_prior()
#  uniform_prior()

## -----------------------------------------------------------------------------
is(normal_prior(mu = 0, sigma = 100), "Prior")

## ----fig.align = "center", fig.dim = c(6, 3)----------------------------------
plot(
  normal_prior(mu = 0, sigma = 10),
  xlim = c(-100, 100),
  ylim = c(0, 0.1)
)

## ----fig.align = "center", fig.dim = c(6, 4)----------------------------------
plot(
  normal_prior(mu = 0, sigma = 10000),
  xlim = c(-100, 100),
  ylim = c(0, 0.1)
)

## ----fig.align = "center", fig.dim = c(5, 3)----------------------------------
plot(
  gamma_prior(alpha = 0.001, beta = 0.001),
  xlim = c(-1, 20),
  ylim = c(0, .025)
)

## ----fig.align = "center", fig.dim = c(5, 4)----------------------------------
plot(
  gamma_prior(alpha = 1, beta = 0.001),
  xlim = c(-1, 20),
  ylim = c(0, .025)
)

## ----message = FALSE----------------------------------------------------------
head(example_matrix[, c("ext", "trt", "time", "cnsr")])

## ----include = FALSE----------------------------------------------------------
example_dataframe <- as.data.frame(example_matrix)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(gtsummary)
tbl_cross(
  example_dataframe,
  trt,
  ext,
  label = list(
    trt ~ "Experimental treatment flag",
    ext ~ "External trial data flag"
  )
)

## ----message = FALSE, echo = FALSE, fig.align = "center", fig.dim = c(7, 4)----
library(survival)
library(survminer)
km_fit <- survfit(Surv(time = time, event = 1 - cnsr) ~ trt + ext, example_dataframe)
ggsurvplot(fit = km_fit, data = example_dataframe, palette = "Set1")

## ----message = FALSE, echo = FALSE--------------------------------------------
library(broom)
library(dplyr)
library(tidyselect)
cox_fit <- coxph(Surv(time = time, event = 1 - cnsr) ~ trt,
  data = example_dataframe,
  subset = ext == 0
)

tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) %>%
  gt() %>%
  fmt_number(columns = 2:7, decimals = 2)

## ----eval = FALSE-------------------------------------------------------------
#  create_analysis_obj(
#    data_matrix,
#    outcome,
#    borrowing,
#    treatment
#  )

## ----echo = FALSE-------------------------------------------------------------
gt(
  tribble(
    ~Argument, ~Description,
    "data_matrix", paste0(
      "The data matrix, including all relevant outcome variables,",
      "and treatment arm and external control arm flags."
    ),
    "outcome", paste0(
      "Object of class Outcome as output by exp_surv_dist(), ",
      "weib_ph_surv_dist(), or logistic_bin_outcome()."
    ),
    "borrowing", "Object of class Borrowing as output by borrowing_details().",
    "treatment", "Object of class Treatment as output by treatment_details()."
  )
) %>%
  tab_style(
    locations = cells_body(columns = "Argument"),
    style = list(
      cell_text(weight = "bold")
    )
  )

## ---- echo = FALSE------------------------------------------------------------
gt(
  tribble(
    ~Constructor, ~Description,
    "exp_surv_dist()", "Exponential survival distribution",
    "weib_ph_surv_dist()", "Weibull survival distribution (proportional hazards formulation)",
    "logistic_bin_outcome()", "Bernoulli distribution with logit parametrization"
  )
) %>%
  tab_style(
    locations = cells_body(columns = "Constructor"),
    style = list(
      cell_text(weight = "bold")
    )
  )

## ----eval = FALSE-------------------------------------------------------------
#  exp_surv_dist(
#    time_var,
#    cens_var,
#    baseline_prior
#  )

## -----------------------------------------------------------------------------
exp_outcome <- exp_surv_dist(
  time_var = "time",
  cens_var = "cnsr",
  baseline_prior = normal_prior(0, 10000)
)

## -----------------------------------------------------------------------------
class(exp_outcome)

## -----------------------------------------------------------------------------
is(exp_outcome, "Outcome")

## ----eval = FALSE-------------------------------------------------------------
#  borrowing_details(
#    method,
#    ext_flag_col,
#    tau_prior
#  )

## -----------------------------------------------------------------------------
bdb_borrowing <- borrowing_details(
  method = "BDB",
  ext_flag_col = "ext",
  tau_prior = gamma_prior(alpha = 0.001, beta = 0.001)
)

## -----------------------------------------------------------------------------
class(bdb_borrowing)

## ----eval = FALSE-------------------------------------------------------------
#  treatment_details(
#    trt_flag_col,
#    trt_prior
#  )

## -----------------------------------------------------------------------------
trt_details <- treatment_details(
  trt_flag_col = "trt",
  trt_prior = normal_prior(0, 10000)
)

## -----------------------------------------------------------------------------
class(trt_details)

## ----message = FALSE----------------------------------------------------------
analysis_object <- create_analysis_obj(
  data_matrix = example_matrix,
  outcome = exp_outcome,
  borrowing = bdb_borrowing,
  treatment = trt_details
)

## -----------------------------------------------------------------------------
class(analysis_object)

## ----message = FALSE----------------------------------------------------------
analysis_object <- create_analysis_obj(
  data_matrix = example_matrix,
  outcome = exp_surv_dist(
    time_var = "time",
    cens_var = "cnsr",
    baseline_prior = normal_prior(0, 10000)
  ),
  borrowing = borrowing_details(
    method = "BDB",
    ext_flag_col = "ext",
    tau_prior = gamma_prior(alpha = 0.001, beta = 0.001)
  ),
  treatment = treatment_details(
    trt_flag_col = "trt",
    trt_prior = normal_prior(0, 10000)
  )
)

## -----------------------------------------------------------------------------
print(analysis_object)

## ----include = FALSE----------------------------------------------------------
results <- mcmc_sample(
  analysis_object,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 1
)

## ----eval = FALSE-------------------------------------------------------------
#  results <- mcmc_sample(
#    analysis_object,
#    iter_warmup = 1000,
#    iter_sampling = 1000,
#    chains = 1
#  )

## -----------------------------------------------------------------------------
results

