# Plot Probability Density Function Values

Plot Probability Density Function Values

## Usage

``` r
plot_pdf(x, y, ...)
```

## Arguments

- x:

  values

- y:

  probability density values `y = f(x)`

- ...:

  passed to
  [`plot()`](https://genentech.github.io/psborrow2/reference/plot.md)

  Plots the density values as a curve with the lower vertical limit set
  to 0.

## Value

No return value, this function generates a plot in the current graphics
device.

## Examples

``` r
x <- seq(-2, 2, len = 100)
y <- dnorm(x)
plot_pdf(x, y)
```
