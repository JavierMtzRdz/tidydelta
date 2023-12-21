
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidydelta

<!-- badges: start -->
<!-- badges: end -->

Delta Method implementation to estimate standard errors in a {tidyverse}
workflow.

## Installation

You can install the development version of tidydelta from
[GitHub](https://github.com/JavierMtzRdz/tidydelta) with:

``` r
remotes::install_github("JavierMtzRdz/tidydelta")
# Or
devtools::install_github("JavierMtzRdz/tidydelta")  
```

## Example

Using tidydelta_m(), the following commands are equivalent:

``` r
# Load packages
library(tidydelta)
library(tidyverse)
#> Warning: package 'dplyr' was built under R version 4.3.1
#> Warning: package 'stringr' was built under R version 4.3.1
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Simulate samples
set.seed(547)
x = rnorm(10000, mean = 5, sd = 2)
y = rnorm(10000, mean = 15, sd = 3)

bd <- tibble(x, y)

# Equivalent uses of tidydelta_m()
tidydelta_m(~ y/x,
            conf_lev = .95)
#> # A tibble: 1 × 6
#>       y     x   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  15.0  5.02  2.99  1.33    0.378     5.61

tidydelta_m(~ bd$y/bd$x,
            conf_lev = .95)
#> # A tibble: 1 × 6
#>       y     x   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  15.0  5.02  2.99  1.33    0.378     5.61
bd %>%
  summarise(tidydelta_m(~ y/x,
                        conf_lev = .95))
#> # A tibble: 1 × 6
#>       y     x   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  15.0  5.02  2.99  1.33    0.378     5.61
```

Now, the data frame is divided into samples to compare the
transformation of the sample with the estimation of `tidydelta()`. In
the real world, you would not need to compute the Delta Method if you
have many samples, but it shows how it can be incorporated in a workflow
with tidyverse.

``` r
(result <- bd %>%
  summarise(tidydelta_m(~ x/y,
                        conf_lev = .95)))
#> # A tibble: 1 × 6
#>       x     y   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  5.02  15.0 0.334 0.149   0.0422    0.626

ggplot() +
  geom_histogram(data = bd %>%
                   mutate(t = x/y),
                 aes(x = t)) +
  geom_vline(aes(xintercept = result$T_n,
                 color = "T_n")) +
  geom_vline(aes(xintercept = c(result$lower_ci,
                                result$upper_ci),
                 color = "CI"),
             linetype = "dashed") +
  labs(color = element_blank()) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
