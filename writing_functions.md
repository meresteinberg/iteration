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

    ##  [1]  1.96227398  0.29990071 -0.01366225  0.27930569  0.39093530 -0.20369765
    ##  [7] -1.37884288  0.88916763  0.02669518 -1.63941674  0.05341588 -0.21417443
    ## [13]  0.80275531  0.31016634  2.00848524 -0.35042367 -1.64904932  0.69930975
    ## [19] -1.57892698  0.12236551 -0.56221258 -0.96702956 -1.00586635  0.81132559
    ## [25]  0.90720032

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

    ##  [1]  1.96227398  0.29990071 -0.01366225  0.27930569  0.39093530 -0.20369765
    ##  [7] -1.37884288  0.88916763  0.02669518 -1.63941674  0.05341588 -0.21417443
    ## [13]  0.80275531  0.31016634  2.00848524 -0.35042367 -1.64904932  0.69930975
    ## [19] -1.57892698  0.12236551 -0.56221258 -0.96702956 -1.00586635  0.81132559
    ## [25]  0.90720032

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

## A new function

``` r
mean_and_sd= function(x) {
  
  mean_x=mean(x)
  sd_x=sd(x)
  
  out_df=
    tibble(
      mean=mean_x,
      sd=sd_x
    )
  
  return(out_df)
  

}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.22  2.97

## Checks stuff using a simulation

``` r
sim_df=
  tibble(
    x=rnorm(30,10,5)
  )

sim_df |> 
  summarize(
    mean= mean(x),
    sd= sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.88  5.43

Simulation function to check sample mean and sd

``` r
sim_mean_sd= function(samp_size, true_mean, true_sd) {
  
  sim_df=
  tibble(
    x=rnorm(samp_size,true_mean,true_sd)
  )

out_df=
  sim_df |> 
  summarize(
    mean= mean(x),
    sd= sd(x)
  )

return(out_df)

}
sim_mean_sd(samp_size=30, true_mean=4, true_sd=12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.38  9.78

``` r
sim_mean_sd(true_mean=4, true_sd=12, samp_size=30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.28  13.8

``` r
sim_mean_sd(30,16,2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  15.9  2.48

can move around arguments if naming them. if not they will go along with
how you originally designed the function. you can also name default
values in the function().
