writing_functions
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
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

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## start small

Everyone loves z scores

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.31204624  0.38139685 -1.28185526  0.88087176  0.28812758  0.64931687
    ##  [7] -1.13016064 -1.28771794  0.16498775 -0.70919443  1.68552348 -0.25596399
    ## [13] -1.53458902 -0.68841594  1.01418844 -0.01377983 -0.74507475  1.74509736
    ## [19]  0.12750809 -0.60231260

Wrote function to compute z score (what we did above)

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if(length(x)<5) {
    stop("only compute z scores when the input has 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

Let’s try our function …

``` r
z_scores(x = x_vec)
```

    ##  [1]  1.31204624  0.38139685 -1.28185526  0.88087176  0.28812758  0.64931687
    ##  [7] -1.13016064 -1.28771794  0.16498775 -0.70919443  1.68552348 -0.25596399
    ## [13] -1.53458902 -0.68841594  1.01418844 -0.01377983 -0.74507475  1.74509736
    ## [19]  0.12750809 -0.60231260

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -1.243559531 -1.273107762  0.327767285  1.624465710 -0.427109847
    ##   [6]  0.614207833  1.083628589 -1.813718930  0.974125352 -0.321740745
    ##  [11]  1.381682436 -0.098008873  0.747889398 -1.797356214 -2.254859991
    ##  [16]  0.646255988  0.547319770  0.868051901  0.459296423  0.159027761
    ##  [21]  0.858284694 -0.512721612 -1.024324189  0.776534841  1.709367878
    ##  [26]  0.508373162  0.427640677  0.177535580 -1.051270798 -1.284957978
    ##  [31] -1.595777585 -1.574544691 -0.639057328  0.186101209  0.802749130
    ##  [36] -0.040027930  0.154607696 -1.200866908 -0.707596835 -1.394368268
    ##  [41] -1.207857697  0.907954549 -0.349158220  0.806097338  0.122354713
    ##  [46]  1.132472335  2.806858018 -0.629951534  1.171798149  1.000091727
    ##  [51] -0.452138006  1.735041321  1.383456132  1.591210690  0.485814614
    ##  [56] -1.007718614  0.434619588  0.330960403 -1.279701907 -0.910193823
    ##  [61]  0.188411455  0.733957697 -0.389832853  0.172015147  0.747338922
    ##  [66] -1.423391102 -0.287901584 -1.273910691  0.675603659  0.195931163
    ##  [71]  0.924504028 -0.926805151 -1.088254489 -0.701467792  0.823678576
    ##  [76]  0.969681429 -0.984998409  1.274787390  0.798568684 -0.553886787
    ##  [81] -0.747053669 -0.638443008 -0.715084226 -0.601740162  0.040915116
    ##  [86] -0.389518393  2.110855660  0.490530392  0.245135786 -1.112380111
    ##  [91]  0.236925637 -0.981663881  1.341548792  1.604486355  1.219605971
    ##  [96]  0.092580953 -0.223626769  1.357540420  0.255445939  0.461895618
    ## [101] -0.355425489 -1.209484920 -0.830557833  1.661911400  0.796357645
    ## [106] -1.605959892 -2.282559179  1.372043538 -0.233204375 -1.034116233
    ## [111] -1.096870518 -0.882076733  0.257299990  0.409619461  0.031015769
    ## [116]  0.001319403 -0.190310828 -0.186879723 -1.363308891 -0.358986752
    ## [121] -0.130992803  0.232998566  0.224235616

Let’s break our function

``` r
z_scores(3) # can't compute z score on one number
```

    ## Error in z_scores(3): only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is jeff") # input has to be numeric
```

    ## Error in z_scores("my name is jeff"): The input x should be numeric

## Let’s compute some stuff

lets compute and return mean and sd of a numeric vector

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if(length(x)<5) {
    stop("only compute z scores when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE) # these are things that get returned
  sd_x = sd(x, na.rm = TRUE) # these are things that get returned
  
  tibble(
    mean_x, sd_x
    )
}
```

## Make up data ..

Let’s *simulate* data

``` r
sim_df = 
  tibble(
    x = rnorm(n=30, mean = 3, sd = 2)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.13      2.14

Write function to do simulations  

the inputs are

- `n_subj` is number of subjects
- `mu` is the true mean
- `sigma` is the true sd

Function simulates data from a normal distribution and computes sample
mean and sd.

``` r
sim_mean_sd = function(n_subj, mu = 3, sigma = 2) {
 
   sim_df = 
      tibble(
        x = rnorm(n=n_subj, mean = mu, sd = sigma)
      )
    
    sim_df |> 
      summarize(
        mu_hat = mean(x),
        sigma_hat = sd(x)
  )
}
```

let’s run this function

``` r
sim_mean_sd(n_subj = 50) # if you just put 50, 50 is value for argument that doesn't have a default
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.07      1.75

IMport the lotr data

``` r
fellowship_ring = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "Fellowship of the Ring")

two_towers = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "Two Towers")

return_ring = 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "Return of the Ring")

lotr_df = 
  bind_rows(fellowship_ring, two_towers, return_ring)
```

the stuff that changes from one line to next are the inputs -\> cell
names and movie title.

``` r
lotr_import = function(cell_range, movie_title) {
  df = 
  read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
  mutate(movie = movie_title)
  
  df
}

fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
tow_towers = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return = lotr_import(cell_range = "J3:L6", movie_title = "Return")

bind_rows(fellowship, two_towers, return)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers
    ## 7 Elf       183   510 Return    
    ## 8 Hobbit      2  2673 Return    
    ## 9 Man       268  2459 Return

Look at one more example

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

write an import function

importin different table numbers

``` r
nsduh_import = function(html, table_num){
  
  
  data = 
  html |> 
  html_table() |> 
  nth(table_num) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
}

nsduh_import(nsduh_html, table_num = 1)
```
