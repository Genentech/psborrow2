# Build the model string by interpolating the Stan template

Build the model string by interpolating the Stan template

## Usage

``` r
build_model_string(
  template_domain,
  template_filename,
  outcome,
  borrowing,
  analysis_obj,
  ...
)
```

## Arguments

- template_domain:

  Character string specifying the domain of the template (e.g., "surv",
  "bin", "cont")

- template_filename:

  Character string specifying the filename of the Stan template

- outcome:

  `Outcome` object

- borrowing:

  `Borrowing` object

- analysis_obj:

  `Analysis` object

- ...:

  Additional named arguments to be passed for interpolation

## Value

String containing the interpolated Stan model
