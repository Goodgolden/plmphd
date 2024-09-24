
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plmphd

<!-- badges: start -->
<!-- badges: end -->

The goal of plmphd is to …

## Installation

You can install the development version of plmphd from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Goodgolden/plmphd")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(plmphd)
#> Loading required package: brokenstick
#> Loading required package: optimx
#> Loading required package: broom.mixed
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: gamlss
#> Loading required package: splines
#> Loading required package: gamlss.data
#> 
#> Attaching package: 'gamlss.data'
#> The following object is masked from 'package:datasets':
#> 
#>     sleep
#> Loading required package: gamlss.dist
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> The following object is masked from 'package:optimx':
#> 
#>     coef<-
#> Loading required package: parallel
#>  **********   GAMLSS Version 5.4-22  **********
#> For more on GAMLSS look at https://www.gamlss.com/
#> Type gamlssNews() to see new features/changes/bug fixes.
#> Loading required package: here
#> here() starts at /Users/goodgolden5/Desktop/plmphd
#> Loading required package: lme4
#> Loading required package: Matrix
#> 
#> Attaching package: 'lme4'
#> The following object is masked from 'package:gamlss':
#> 
#>     refit
#> The following object is masked from 'package:nlme':
#> 
#>     lmList
#> Loading required package: MASS
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Loading required package: matrixcalc
#> Loading required package: plotly
#> Loading required package: ggplot2
#> 
#> Attaching package: 'plotly'
#> The following object is masked from 'package:ggplot2':
#> 
#>     last_plot
#> The following object is masked from 'package:MASS':
#> 
#>     select
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:graphics':
#> 
#>     layout
#> Loading required package: shiny
#> Loading required package: tibble
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following objects are masked from 'package:Matrix':
#> 
#>     expand, pack, unpack
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ forcats   1.0.0     ✔ readr     2.1.5
#> ✔ lubridate 1.9.3     ✔ stringr   1.5.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ nlme::collapse() masks dplyr::collapse()
#> ✖ tidyr::expand()  masks Matrix::expand()
#> ✖ plotly::filter() masks dplyr::filter(), stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ tidyr::pack()    masks Matrix::pack()
#> ✖ plotly::select() masks MASS::select(), dplyr::select()
#> ✖ tidyr::unpack()  masks Matrix::unpack()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
#> 
#>  Welcome to my package; this is a package
#>                         developed for Randy Jin's MS thesis
#> 
#> 
#> Attaching package: 'plmphd'
#> 
#> 
#> The following objects are masked from 'package:brokenstick':
#> 
#>     brokenstick, control_kr, set_control
#> 
#> 
#> The following object is masked from 'package:base':
#> 
#>     match
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
