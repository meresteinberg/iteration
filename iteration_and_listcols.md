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
    ##   [1] 0.420839896 0.872120088 0.314779987 0.521769474 0.901574242 0.895707120
    ##   [7] 0.704217245 0.345690138 0.137432153 0.149438988 0.230468593 0.072512813
    ##  [13] 0.520075565 0.137896161 0.398192412 0.586492748 0.878587630 0.997435656
    ##  [19] 0.361046509 0.182921033 0.085485985 0.285539737 0.882340717 0.707271110
    ##  [25] 0.687934492 0.541234854 0.483427830 0.385004369 0.853616739 0.393512230
    ##  [31] 0.337215769 0.341910923 0.637676240 0.521182395 0.812370218 0.798033373
    ##  [37] 0.807323082 0.128506172 0.990637983 0.118658071 0.993566855 0.379748572
    ##  [43] 0.812292921 0.905289900 0.860715372 0.589765123 0.912141494 0.187664083
    ##  [49] 0.002966861 0.366773545 0.561618460 0.218691235 0.607429186 0.433142233
    ##  [55] 0.545486921 0.391027796 0.230699007 0.215583938 0.426055945 0.205089241
    ##  [61] 0.067067907 0.296686453 0.936945955 0.222116111 0.947399912 0.479819498
    ##  [67] 0.768175548 0.409446775 0.590043737 0.039699469 0.127996247 0.120998196
    ##  [73] 0.593129576 0.346877872 0.539412868 0.006714958 0.636818771 0.294144247
    ##  [79] 0.010809204 0.836356263 0.864888057 0.248841656 0.660744671 0.030809342
    ##  [85] 0.579393629 0.958281527 0.181874836 0.200460145 0.366477397 0.711056711
    ##  [91] 0.528070825 0.009210661 0.197169473 0.532399144 0.438870596 0.705555423
    ##  [97] 0.471507767 0.583685972 0.517988845 0.553367291
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.57674 -0.79527 -0.09282 -0.07556  0.60892  3.88364

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

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.57674 -0.79527 -0.09282 -0.07556  0.60892  3.88364

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

    ##  [1] -0.8117978  2.6359516 -3.3885636  0.5814337 -6.6965865 11.2189816
    ##  [7]  7.9057304  8.5791380 -3.5600558  3.4964334 -1.0919507 -4.9521928
    ## [13] -5.2447927 -0.5601263  3.9850697 -1.0178379 -2.6071286 -6.9241312
    ## [19]  4.6161135  0.7441460

``` r
list_norm[["b"]]
```

    ##  [1]  4.3359929  1.7532231  5.8376599 -5.1398248  8.3988651 -0.8872901
    ##  [7]  8.1090155  4.4386403  6.3524872 -5.7511966  4.0874419  3.0185455
    ## [13] 10.3067754  8.0932604  3.9531309  0.3964295  2.0478619 -4.0504410
    ## [19]  4.4814652  5.7615091

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
    ## 1 0.345  5.11

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.28  4.50

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.90  10.9

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.33  7.59

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
    ## 1 0.345  5.11
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.28  4.50
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.90  10.9
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.33  7.59
