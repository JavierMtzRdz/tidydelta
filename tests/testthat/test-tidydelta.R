# Load packages
library(tidyverse)

# Simulate samples
set.seed(547)
x <<- rnorm(10000, mean = 5, sd = 2)
y <<- rnorm(10000, mean = 15, sd = 3)

x_exp <<- rexp(10000, 2)

y_exp <<- rexp(10000, 3)

bd <<- tibble::tibble(x, y)

tible_test <<- tibble::tibble(
  y = 15.01,
  x = 5.016,
  T_n = 2.992,
  se = 1.334,
  lower_ci = 0.378,
  upper_ci = 5.607
)

tible_test_80 <<- tibble::tibble(
  y = 15.01,
  x = 5.016,
  T_n = 2.992,
  se = 1.334,
  lower_ci = 1.283,
  upper_ci = 4.702
)

test_that("Test formula to an expression", {
  expect_equal(
    class(for_to_exp(~ y / x)),
    "call"
  )

  expect_equal(
    class(for_to_exp(~ bd$y / bd$x)),
    "call"
  )
})

test_that("Test extraction from formula", {
  expect_equal(
    ext_bd_var(~ bd$y / bd$x),
    c(y = "bd$y", x = "bd$x")
  )
})


test_that("Test error in extraction of variables and names", {
  expect_error(
    cases_ext(1 + 5),
    "The function does not meet the differentiability criteria."
  )
  expect_error(
    cases_ext("x"),
    "The function does not meet the differentiability criteria."
  )
  expect_error(
    cases_ext(
      ~ y / x,
      1
    ),
    "You need to add the covariance matrix in 'cov_dta'."
  )
})


test_that("Test extraction of variables and names", {
  expect_equal(
    cases_ext(~ bd$y / bd$x),
    list(
      "vars" = bd %>% select(y, x),
      "fun_exp" = for_to_exp(~ y / x)
    )
  )
  expect_equal(
    cases_ext(~ y / x),
    list(
      "vars" = bd %>% select(y, x),
      "fun_exp" = for_to_exp(~ y / x)
    )
  )
})

test_that("Test warnings and errors in the implementation", {
  expect_warning(
    tidydelta(~ y_exp / x_exp),
    "The provided variables do not meet the normality criteria."
  )
  expect_error(
    tidydelta(~ x * 0),
    "The function does not meet the differentiability criteria."
  )
})


test_that("Test implementation", {
  expect_equal(
    round(tidydelta(~ bd$y / bd$x), 3),
    tible_test
  )
  expect_equal(
    round(tidydelta(~ y / x), 3),
    tible_test
  )
  expect_equal(
    round(bd %>% summarise(tidydelta(~ y / x)), 3),
    tible_test
  )
  expect_equal(
    round(suppressWarnings(tidydelta(~ y / x,
            conf_lev = 0.8
          )), 3),
    tible_test_80
  )
})
