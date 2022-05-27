
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aquaman

<!-- badges: start -->
<!-- badges: end -->

## WORK IN PROGRESS - DO NOT USE IN PRODUCTION

The goal of aquaman is to assess the size of the mixing zone from
transect based ecology sampling.

## Installation

You can install the development version of aquaman like so:

``` r
install.packages("devtools")
devtools::install_github("aquametrics/aquaman")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(aquaman)
```

``` r
## Run the demo data and return the area of the mixing zone
area <- assess(demp_iqi)
```

Here’s an example of input data required:

``` r
demo_iqi
#> # A tibble: 30 × 15
#>    Survey_date         MCFF      Transect Station   IQI Easting Northing
#>    <dttm>              <chr>        <dbl>   <dbl> <dbl>   <dbl>    <dbl>
#>  1 2021-05-26 00:00:00 Bellister        1       1  0.4   449114  1161084
#>  2 2021-05-26 00:00:00 Bellister        1       2  0.59  449093  1161101
#>  3 2021-05-26 00:00:00 Bellister        1       3  0.61  449080  1161123
#>  4 2021-05-26 00:00:00 Bellister        1       4  0.56  449070  1161126
#>  5 2021-05-26 00:00:00 Bellister        1       5  0.56  449059  1161130
#>  6 2021-05-26 00:00:00 Bellister        1       6  0.53  449014  1161163
#>  7 2021-05-26 00:00:00 Bellister        1       7  0.67  448951  1161214
#>  8 2021-05-26 00:00:00 Bellister        1       8  0.68  448898  1161259
#>  9 2021-05-26 00:00:00 Bellister        1       9  0.67  448929  1161312
#> 10 2021-05-26 00:00:00 Bellister        2       1  0.38  449308  1161055
#> # … with 20 more rows, and 8 more variables: MCFF_Transect <chr>,
#> #   Longitude <dbl>, Latitude <dbl>, Bearing <dbl>, Distance <dbl>,
#> #   `Number of stations per transect` <dbl>, `WFD status` <chr>,
#> #   MCFF_Transect_Station <chr>
```
