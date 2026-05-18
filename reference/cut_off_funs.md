# Cut Off Functions

Cut Off Functions

## Usage

``` r
cut_off_none()

cut_off_after_first(time)

cut_off_after_last(time)

cut_off_after_events(n)
```

## Arguments

- time:

  Time to cut off

- n:

  Number of events

## Value

A `DataSimCutOff` object containing a cut-off function

## Functions

- `cut_off_none()`: No cut off is specified

- `cut_off_after_first()`: Cut off at `time` after first enrolled
  patient

- `cut_off_after_last()`: Cut off at `time` after last enrolled patient

- `cut_off_after_events()`: Cut off after the time of the n-th event

## Examples

``` r
cut_off_none()
#> An object of class "DataSimCutOff"
#> Slot "fun":
#> function (data) 
#> {
#>     data
#> }
#> <bytecode: 0x55e4a29ccf30>
#> <environment: 0x55e4a29cd0b8>
#> 
#> Slot "label":
#> [1] "No cut off"
#> 
cut_off_after_first(time = 36)
#> An object of class "DataSimCutOff"
#> Slot "fun":
#> function (data) 
#> {
#>     cut_time <- min(data$enrollment) + time
#>     after_cut_off <- data$enrollment + data$eventtime > cut_time
#>     data$status <- ifelse(after_cut_off, 0, data$status)
#>     data$eventtime <- ifelse(after_cut_off, cut_time - data$enrollment, 
#>         data$eventtime)
#>     data[data$enrollment < cut_time, ]
#> }
#> <bytecode: 0x55e4a2960c00>
#> <environment: 0x55e4a295e5e0>
#> 
#> Slot "label":
#> Cut off after first enrolled patient reaches time = 36
#> 
cut_off_after_last(time = 36)
#> An object of class "DataSimCutOff"
#> Slot "fun":
#> function (data) 
#> {
#>     cut_time <- max(data$enrollment) + time
#>     after_cut_off <- data$enrollment + data$eventtime > cut_time
#>     data$status <- ifelse(after_cut_off, 0, data$status)
#>     data$eventtime <- ifelse(after_cut_off, cut_time - data$enrollment, 
#>         data$eventtime)
#>     data
#> }
#> <bytecode: 0x55e4a25f1780>
#> <environment: 0x55e4a25eef30>
#> 
#> Slot "label":
#> Cut off after last enrolled patient reaches time=36
#> 
cut_off_after_events(n = 20)
#> An object of class "DataSimCutOff"
#> Slot "fun":
#> function (data) 
#> {
#>     data_s1 <- data[data$status == 1, ]
#>     cut_time <- min(sort(data_s1$enrollment + data_s1$eventtime)[n], 
#>         Inf, na.rm = TRUE)
#>     after_cut_off <- data$enrollment + data$eventtime > cut_time
#>     data$status <- ifelse(after_cut_off, 0, data$status)
#>     data$eventtime <- ifelse(after_cut_off, cut_time - data$enrollment, 
#>         data$eventtime)
#>     data[data$enrollment < cut_time, ]
#> }
#> <bytecode: 0x55e4a254f050>
#> <environment: 0x55e4a254cfe0>
#> 
#> Slot "label":
#> Cut off after 20 events
#> 
```
