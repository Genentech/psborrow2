# Example data matrix

A matrix containing data from a clinical trial with a treatment arm and
a control arm, as well as data from an external control. In this
simulated dataset, the true hazard ratio (HR) for the time-to-event
endpoint comparing the experimental treatment to the control treatment
is 0.70. The true odds ratio (OR) for the binary response endpoint
comparing the experimental treatment to the control treatment is 1.20.

## Usage

``` r
example_matrix
```

## Format

A data frame with 500 rows and 11 columns. The distributions of patients
is: 50 internal control patients, 100 internal experimental patients,
350 external control patients.

- id:

  patient identifier

- ext:

  0/1, flag for external controls

- trt:

  0/1, flag for treatment arm

- cov1:

  0/1, baseline covariate

- cov2:

  0/1, baseline covariate

- cov3:

  0/1, baseline covariate

- cov4:

  0/1, baseline covariate

- time:

  numeric \>0, survival time

- status:

  0/1, indicator for event status (1 = had event, 0 = did not have
  event)

- cnsr:

  0/1, censoring indicator (1 = was censored, 0 = was not censored)

- resp:

  0/1, indicator for response outcome (1 = had a response, 0 = did not
  have a response)
