writing_functions
================
2024-10-25

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## writing my first function!

as an exampl,e heres a z-score computation

``` r
x_vec= rnorm(n=25, mean=10, sd=3.5)

(x_vec-mean(x_vec))/sd(x_vec)
```

    ##  [1] -1.05229646 -0.10854792  0.37591985  1.67308372  0.02687013  0.34891038
    ##  [7]  0.90996823 -0.72796074  0.24624535  1.01944696 -0.23241248  0.24076109
    ## [13] -1.77253655  0.15275309  0.03884428 -0.83882203  1.24422956 -0.25207722
    ## [19] -0.32489194 -2.04672521 -0.99586073 -1.35386942  1.83520653  0.86277224
    ## [25]  0.73098929

Now ill write a function to do this

``` r
z_scores= function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numberic")
  }
  
  if(length(x)<5) {
    stop("you need at least five numbers to compute the z score")
  }
  
  
  z=(x-mean(x))/sd(x)
  
  return(z)
  
}

z_scores(x=x_vec)
```

    ##  [1] -1.05229646 -0.10854792  0.37591985  1.67308372  0.02687013  0.34891038
    ##  [7]  0.90996823 -0.72796074  0.24624535  1.01944696 -0.23241248  0.24076109
    ## [13] -1.77253655  0.15275309  0.03884428 -0.83882203  1.24422956 -0.25207722
    ## [19] -0.32489194 -2.04672521 -0.99586073 -1.35386942  1.83520653  0.86277224
    ## [25]  0.73098929

saying x=x_vec allows you to put x_vec anywhere in function there is an
x

does this always work?

``` r
z_scores(x=3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x= c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numberic

x=3 doesnt work bc cant compute sd of one number (see conditional
function added) cant do mean of words (also see conditional function we
put in for error if !is numeric)
