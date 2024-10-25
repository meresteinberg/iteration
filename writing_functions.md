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

``` r
library(readxl)
```

## writing my first function!

as an exampl,e heres a z-score computation

``` r
x_vec= rnorm(n=25, mean=10, sd=3.5)

(x_vec-mean(x_vec))/sd(x_vec)
```

    ##  [1] -0.09793990 -0.15919770  0.26017156 -0.26044186  0.61990651 -1.16363263
    ##  [7]  0.75015696  2.29649156 -1.11168066  0.28664443 -0.01123936  0.50338884
    ## [13]  1.01311900  0.03433641 -0.09418220  0.18491960  2.02074811 -0.54128262
    ## [19] -2.19604458  0.68628240  0.27456824 -0.94853706 -1.64131231 -0.24143385
    ## [25] -0.46380891

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

    ##  [1] -0.09793990 -0.15919770  0.26017156 -0.26044186  0.61990651 -1.16363263
    ##  [7]  0.75015696  2.29649156 -1.11168066  0.28664443 -0.01123936  0.50338884
    ## [13]  1.01311900  0.03433641 -0.09418220  0.18491960  2.02074811 -0.54128262
    ## [19] -2.19604458  0.68628240  0.27456824 -0.94853706 -1.64131231 -0.24143385
    ## [25] -0.46380891

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
    ## 1  9.82  3.94

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
    ## 1  8.45  4.30

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
    ## 1  4.14  12.6

``` r
sim_mean_sd(true_mean=4, true_sd=12, samp_size=30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.96  14.0

``` r
sim_mean_sd(30,16,2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.9  1.96

can move around arguments if naming them. if not they will go along with
how you originally designed the function. you can also name default
values in the function().

\##Revisit LoTR words

``` r
fellowship_df=
  read_excel("data/LotR_Words.xlsx", range= "B3:D6") |> 
  mutate(movie= "fellowship") |> 
  janitor::clean_names()

two_towers_df=
  read_excel("data/LotR_Words.xlsx", range= "F3:H6") |> 
  mutate(movie= "two_towers") |> 
    janitor::clean_names()


return_king_df=
  read_excel("data/LotR_Words.xlsx", range= "J3:L6") |> 
  mutate(movie= "return_king") |> 
    janitor::clean_names()
```

Lets write a function for this instead of doing it for every table/
–think about what changed for each input

``` r
lotr_import= function(cell_range, movie_title) {
 
movie_df= 
  read_excel("data/LotR_Words.xlsx", range= cell_range) |> 
  mutate(movie= movie_title) |> 
  janitor::clean_names() |> 
  pivot_longer(
    female:male,
    names_to= "sex",
    values_to = "words"
  ) |> 
  select(movie, everything())
  
  
return(movie_df)
}

lotr_import(cell_range = "B3:D6", movie_title = "fellowship")
```

    ## # A tibble: 6 × 4
    ##   movie      race   sex    words
    ##   <chr>      <chr>  <chr>  <dbl>
    ## 1 fellowship Elf    female  1229
    ## 2 fellowship Elf    male     971
    ## 3 fellowship Hobbit female    14
    ## 4 fellowship Hobbit male    3644
    ## 5 fellowship Man    female     0
    ## 6 fellowship Man    male    1995

``` r
lotr_import("B3:D6", "fellowship")
```

    ## # A tibble: 6 × 4
    ##   movie      race   sex    words
    ##   <chr>      <chr>  <chr>  <dbl>
    ## 1 fellowship Elf    female  1229
    ## 2 fellowship Elf    male     971
    ## 3 fellowship Hobbit female    14
    ## 4 fellowship Hobbit male    3644
    ## 5 fellowship Man    female     0
    ## 6 fellowship Man    male    1995

``` r
lotr_import("F3:H6", "two_towers")
```

    ## # A tibble: 6 × 4
    ##   movie      race   sex    words
    ##   <chr>      <chr>  <chr>  <dbl>
    ## 1 two_towers Elf    female   331
    ## 2 two_towers Elf    male     513
    ## 3 two_towers Hobbit female     0
    ## 4 two_towers Hobbit male    2463
    ## 5 two_towers Man    female   401
    ## 6 two_towers Man    male    3589

``` r
lotr_import("J3:L6", "return_king")
```

    ## # A tibble: 6 × 4
    ##   movie       race   sex    words
    ##   <chr>       <chr>  <chr>  <dbl>
    ## 1 return_king Elf    female   183
    ## 2 return_king Elf    male     510
    ## 3 return_king Hobbit female     2
    ## 4 return_king Hobbit male    2673
    ## 5 return_king Man    female   268
    ## 6 return_king Man    male    2459

``` r
lotr_df=
  bind_rows(
    lotr_import("B3:D6", "fellowship"),
    lotr_import("F3:H6", "two_towers"),
    lotr_import("J3:L6", "return_king")
  )
```

now if wanted to clean up all tables, can do it in the function

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)


marj_table=
  nsduh_html |> 
  html_table() |> 
  nth(1) |> 
  slice(-1) |> 
  mutate(drug= "marj")

cocaine_table=
  nsduh_html |> 
  html_table() |> 
  nth(4) |> 
  slice(-1) |> 
  mutate(drug= "cocaine")

heroin_table=
  nsduh_html |> 
  html_table() |> 
  nth(5) |> 
  slice(-1) |> 
  mutate(drug= "heroin")
```

Now do with a function instead:

``` r
source("source/nsduh_table_format.R")

drug_import(html=nsduh_html, table=1, drug_name = "marj")
```

    ## # A tibble: 56 × 12
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
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>

``` r
drug_import(nsduh_html, 4, "cocaine")
```

    ## # A tibble: 56 × 12
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
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>

``` r
drug_import(nsduh_html, 5, "marj")
```

    ## # A tibble: 56 × 12
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
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>

``` r
bind_rows(
  drug_import(html=nsduh_html, table=1, drug_name = "marj"),
  drug_import(nsduh_html, 4, "cocaine"),
  drug_import(nsduh_html, 5, "marj")
)
```

    ## # A tibble: 168 × 12
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
    ## # ℹ 158 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>
