## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align = "center",
  fig.width = 5,
  fig.height = 3,
  dpi = 120,
  comment = "#"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(psborrow2)

## -----------------------------------------------------------------------------
uninformative_normal <- normal_prior(0, 10000)
uninformative_normal

## -----------------------------------------------------------------------------
plot(uninformative_normal)

## -----------------------------------------------------------------------------
conservative_tau <- gamma_prior(0.001, 0.001)
aggressive_tau <- gamma_prior(1, 0.001)
plot(aggressive_tau, xlim = c(0, 2000), col = "blue", ylim = c(0, 1e-03))
plot(conservative_tau, xlim = c(0, 2000), col = "red", add = TRUE)

