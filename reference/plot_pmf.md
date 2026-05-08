# Plot Probability Mass Function Values

Plot Probability Mass Function Values

## Usage

``` r
plot_pmf(x, y, ..., col = "grey", add = FALSE)
```

## Arguments

- x:

  values

- y:

  probability mass values `y = f(x)`

- ...:

  passed to
  [`plot()`](https://genentech.github.io/psborrow2/reference/plot.md)
  and [`rect()`](https://rdrr.io/r/graphics/rect.html)

- col:

  Fill color of bars.

- add:

  Add bars to existing plot.

  Plots the probability values as a barplot.

## Value

No return value, this function generates a plot in the current graphics
device.

## Examples

``` r
x <- seq(0, 5)
y <- dpois(x, lambda = 2)
plot_pmf(x, y)
```
