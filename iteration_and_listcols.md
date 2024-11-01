Iteration and Listcols
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

## Here’s some lists

printing “l” gives you the list of each thing l\[\[“mat”\]\]\[1, 3\]
–giving us matrix with 1st row 3rd column

``` r
l=list(
  vec_numeric = 1:4,
  unif_sample= runif(100),
  mat=matrix(1:8, nrow=2, ncol=4, byrow=TRUE),
  summary=summary(rnorm(1000))
  
)
l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.97288718 0.22494217 0.78117741 0.53388081 0.77738190 0.63895658
    ##   [7] 0.58611544 0.03037604 0.07144831 0.24634860 0.04803987 0.73956523
    ##  [13] 0.46352216 0.59804768 0.26413960 0.25763298 0.83066734 0.02756993
    ##  [19] 0.91783702 0.51618853 0.03834828 0.50405498 0.84442455 0.27532749
    ##  [25] 0.65980958 0.69376531 0.99711784 0.12602257 0.48015360 0.79150758
    ##  [31] 0.01420450 0.92504724 0.55190726 0.03831149 0.67247781 0.94760420
    ##  [37] 0.25846106 0.74730090 0.15864028 0.62294942 0.73033733 0.66638598
    ##  [43] 0.18282918 0.78950109 0.26479385 0.42069600 0.04883707 0.88280423
    ##  [49] 0.49442663 0.19340656 0.74822500 0.05579932 0.89056182 0.76038146
    ##  [55] 0.70502616 0.48866723 0.71321029 0.39236638 0.76319380 0.26437724
    ##  [61] 0.78978872 0.89031121 0.02010922 0.59692642 0.54530145 0.09971445
    ##  [67] 0.21459322 0.40779189 0.19119017 0.86127529 0.05875475 0.33350298
    ##  [73] 0.10472042 0.09537840 0.81805541 0.89145290 0.46194405 0.93918790
    ##  [79] 0.06041671 0.87753682 0.74437778 0.94755363 0.34797249 0.38812330
    ##  [85] 0.69066021 0.93979849 0.55218146 0.62997533 0.63864979 0.11452451
    ##  [91] 0.72673816 0.40102771 0.61812945 0.13982638 0.59453423 0.73826060
    ##  [97] 0.79873305 0.38513791 0.04900552 0.55259359
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.831035 -0.677351  0.003362 -0.007869  0.663854  3.169015

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1, 3]
```

    ## [1] 3

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
l[[4]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.831035 -0.677351  0.003362 -0.007869  0.663854  3.169015

Make a list that’s hopefully a bit more useful.

``` r
list_norm=
  list(
    a=rnorm(20, 0, 5 ),
    b=rnorm(20, 5, 5 ),
    c=rnorm(20, 0, 10 ),
    d=rnorm(20, 4, 10 )
  )

list_norm[["a"]]
```

    ##  [1]  1.4554184  3.7018442  5.7610937 10.1536494 -2.8853026  4.8310078
    ##  [7]  8.1101245  7.5248937 -3.8374933 -1.9499245  2.6325759  4.5383215
    ## [13]  0.8852305 -2.0478275 -0.8772028  0.9584205  2.3638314 -2.6949129
    ## [19]  3.7844292 -6.5553662

``` r
list_norm[["b"]]
```

    ##  [1]  2.4070966  0.7357426 -1.5163940  4.0863893  7.2691835  5.5965671
    ##  [7] -0.2720760 -0.4991429  6.2633140  8.2102025 13.0412664 -3.4325136
    ## [13]  0.4523483 -0.6261225 10.1786009 10.7806961  8.1378370  5.2233279
    ## [19]  7.5606860  3.0348699

Let’s reuse the function we wrote last time.

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
```

Let’s use the function to take mean and sd of all samples.

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  4.38

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.33  4.61

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.87  6.66

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.00  9.20

same line of code copied and pasted over and over…

## Use a for loop

create output list, and run a for loop

``` r
output= vector("list", length=4)

for (i in 1:4) {
  
  output[[i]]=mean_and_sd(list_norm[[i]])
  
}
output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  4.38
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.33  4.61
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.87  6.66
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.00  9.20

creating list of 4 mean and sd from list_norm

## Do same thing

but with ‘map’ instead

``` r
output= map(list_norm, mean_and_sd)
output
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  4.38
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.33  4.61
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.87  6.66
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.00  9.20

this map is equivalent to the for loop above

Let’s do a couple of other things

``` r
output= map(list_norm, median)
output
```

    ## $a
    ## [1] 1.909625
    ## 
    ## $b
    ## [1] 4.654859
    ## 
    ## $c
    ## [1] -3.066306
    ## 
    ## $d
    ## [1] 1.138832

can compute median, IQR, etc

``` r
output=
  map(list_norm, mean_and_sd) |> 
  bind_rows()
output
```

    ## # A tibble: 4 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  4.38
    ## 2  4.33  4.61
    ## 3 -1.87  6.66
    ## 4  4.00  9.20

``` r
output=map_dbl(list_norm, IQR)
output=map_dfr(list_norm, IQR)

output
```

    ## # A tibble: 1 × 4
    ##       a     b     c     d
    ##   <dbl> <dbl> <dbl> <dbl>
    ## 1  6.59  7.43  10.5  12.8

## LIST COLUMNS!!

``` r
listcol_df=
  tibble(
    name=c("a", "b", "c", "d"),
    samp= list_norm
  )
listcol_df
```

    ## # A tibble: 4 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>  
    ## 3 c     <dbl [20]>  
    ## 4 d     <dbl [20]>

``` r
listcol_df |> 
  filter(name %in% c("a", "b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df |> 
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
listcol_df[["samp"]][["a"]]
```

    ##  [1]  1.4554184  3.7018442  5.7610937 10.1536494 -2.8853026  4.8310078
    ##  [7]  8.1101245  7.5248937 -3.8374933 -1.9499245  2.6325759  4.5383215
    ## [13]  0.8852305 -2.0478275 -0.8772028  0.9584205  2.3638314 -2.6949129
    ## [19]  3.7844292 -6.5553662

listcol_df\[\[“samp”\]\] –\> this whole thing is just extracting a list
and can access just “a” or

Compute mean and sd

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  4.38

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.33  4.61

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  4.38
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.33  4.61
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.87  6.66
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.00  9.20

ADD A LIST COLUMN!!

``` r
listcol_df |> 
  mutate(
    output=map(samp, mean_and_sd),
    iqr= map_dbl(samp,IQR))
```

    ## # A tibble: 4 × 4
    ##   name  samp         output             iqr
    ##   <chr> <named list> <named list>     <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  6.59
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  7.43
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 10.5 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 12.8

``` r
listcol_df |> 
  mutate(
    output=map(samp, mean_and_sd),
    iqr= map_dbl(samp,IQR)) |> 
  select(-samp) |> 
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name   mean    sd   iqr
    ##   <chr> <dbl> <dbl> <dbl>
    ## 1 a      1.79  4.38  6.59
    ## 2 b      4.33  4.61  7.43
    ## 3 c     -1.87  6.66 10.5 
    ## 4 d      4.00  9.20 12.8

### NSDUH

ideal to extract tables and put everything into one dataset

This is a version of our function last time

``` r
drug_import= function(html, table, drug_name) {
  
  drug_table=
    html |> 
    html_table() |> 
    nth(table) |> 
    slice(-1) |> 
    select(-contains("P Value"))
  
  return(drug_table)
}
```

We need to import the html and then extract the correct tables

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
drug_import(html = nsduh_html, table=1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
drug_import(html = nsduh_html, table=4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
drug_import(html = nsduh_html, table=5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_df=
  tibble(
    drug=c("marj", "cocaine", "heroin"),
    table_n=c(1,4,5)
  ) |> 
  mutate(table= map(table_n, drug_import, html = nsduh_html)) |> 
  unnest(table)

nsduh_df |> 
  filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug    table_n State    `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>     <dbl> <chr>    <chr>            <chr>            <chr>             
    ## 1 marj          1 New York 14.24b           15.04            13.94             
    ## 2 cocaine       4 New York 2.28             2.54             0.71              
    ## 3 heroin        5 New York 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>

``` r
nsduh_df=
  tibble(
    drug=c("marj", "cocaine", "heroin"),
    table_n=c(1,4,5)
  ) |> 
  mutate(table= map(table_n, \(x) drug_import (html = nsduh_html, table=x))) |> 
  unnest(table)
```

unnest will give us the full df so we can see it–now we have a df and
can play around with it

Two other examples on course website..

Lets look at weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/meredithsteinberg/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:18:43.767423 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/meredithsteinberg/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:18:50.009417 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/meredithsteinberg/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:18:51.846163 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

Create a list column

``` r
weather_nest=
  weather_df |> 
  nest(data= date:tmin)
```

``` r
weather_nest[["data"]][[1]]
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

weather_nest\[\[“data”\]\]\[\[1\]\]–looking just at central park

Let’s try regressing tmax on tmin

``` r
lm(tmax~tmin, data= weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax~tmin, data= weather_nest[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax~tmin, data= weather_nest[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

Let’s define a function that fits the regression I want

``` r
weather_lm=function(df) {
  lm(tmax~tmin, data=df)
}
```

``` r
weather_lm(weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
weather_nest |> 
  mutate(model_fit=map(data,weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model_fit
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>     
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>     
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>

``` r
map(weather_nest[["data"]],weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

``` r
weather_nest |> 
  mutate(model_fit=map(data, \(x) lm(tmax~tmin, data= x))) |> 
  pull(model_fit)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

two chunks above doing same thing
