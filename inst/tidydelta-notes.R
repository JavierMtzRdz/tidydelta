# Tidydelta notes
# Setup ----
## Packages to use ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales, mytidyfunctions, presupuestoR, rlang)

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)




bd <- tibble(x = rnorm(1000, mean = 5, sd = 45),
             y = rnorm(1000, mean = 35, sd = 43)) %>%
  mutate(num_g = round(row_number(), -2))

bd %>%
  # filter(num_g == 0) %>%
  group_by(num_g) %>%
  summarise(tidydelta_m(~ x^2 + y,
                        conf_lev = .95))


x = rnorm(1000, mean = 5, sd = 2)
y = rnorm(1000, mean = 15, sd = 3)

bd <- tibble(x, y)

tidydelta_m(~ 1/y + x,
            conf_lev = .95)

tidydelta_m(~ 1/bd$y + bd$x,
            conf_lev = .95)

tidydelta_m(~ x,
            conf_lev = .95)

# Generate data in form of vectors and data frame
set.seed(547)
x = rnorm(10000, mean = 150, sd = 10)
y = rnorm(10000, mean = 135, sd = 13)

bd <- tibble(x, y) %>%
  mutate(num_g = round(row_number(), -2))

# Using tidydelta_m(), the following comands are equivalent
tidydelta_m(~ x/y,
            conf_lev = .95)
#> # A tibble: 1 × 6
#>       x     y   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  150.  135.  1.11 0.129    0.858     1.36

tidydelta_m(~ bd$x/bd$y,
            conf_lev = .95)
#> # A tibble: 1 × 6
#>       x     y   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  150.  135.  1.11 0.129    0.858     1.36

bd %>%
  summarise(tidydelta_m(~ x/y,
              conf_lev = .95))
#> # A tibble: 1 × 6
#>       x     y   T_n    se lower_ci upper_ci
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1  150.  135.  1.11 0.129    0.858     1.36

#
result <- bd %>%
  summarise(tidydelta_m(~ log(x) + 1/y,
                        conf_lev = .95))

ggplot() +
  geom_histogram(data = bd %>%
                   mutate(t = log(x) + 1/y),
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

ggsave(paste0("example.png"),
       bg = "transparent",
       width = 150,                 # Ancho de la gráfica
       height = 70,
       units = "mm",
       dpi = 300)

# Vs direct sd -----
set.seed(547)
bd <- tibble(x = rnorm(10000, mean = 55, sd = 45),
             y = rnorm(10000, mean = 14, sd = 13))
sd(bd$y)

func <- function(x) {
  x <- ifelse(x > 1 & x < 15, NA, 1/x)
  return(x)
}
func(0)

(process <- bd %>%
  summarise(tidydelta_m(~ func(y),
                        conf_lev = .95)))

ggplot() +
  xlim(-30, 30) +
  geom_function(fun = func)

bd <- tibble(x = runif(1000, 15, 45),
             y = runif(1000, 25, 33))

(process <- bd %>%
    summarise(tidydelta_m(~ y * x^2,
                          conf_lev = .95)))

ggplot() +
  geom_histogram(data = bd,
               aes(x =  y * x^2)) +
  geom_vline(xintercept = c(process$lower_ci,
                            process$upper_ci))

sd(bd$x/bd$y)

(process2 <- bd %>%
  summarise(tidydelta_m(~ mean(x)/mean(y),
                        conf_lev = .95)))

boot_fun <- function(data, num, funct = mean, q = 0.95) {
  resamples <- lapply(1:num, function(i) sample(data, 1, replace=T))
  r.transf <- sapply(resamples, funct)
  std.err <- sqrt(var(r.transf))
  lower_ci <- quantile(r.transf, 0.5-q/2)
  upper_ci <- quantile(r.transf, 0.5+q/2)
  list(std.err=std.err, lower_ci = lower_ci, upper_ci = upper_ci)
}
(boot_process <- boot_fun(bd %>%
           mutate(z = x/y) %>% pull(z),
         funct = mean,
         10000))

ggplot() +
  xlim(-10, 16) +
  geom_histogram(data = bd %>%
                   mutate(z = x/y),
                 aes(x = z)) +
  geom_vline(aes(xintercept = c(process2$lower_ci,
                                process2$upper_ci),
                 color = "Delta Method")) +
    geom_vline(aes(xintercept = c(boot_process$lower_ci,
                                  boot_process$upper_ci),
                 color = "Bootstrap"))


mod <- glm(vs ~ mpg + cyl + hp,
           family=binomial, data = mtcars)
pacman::p_load(ggeffects)

ggeffects::ggeffect(mod)
# view results




# Comparation existing implementations vs tidydelta -----

m1 <- lm(y ~ x, data = bd)
m1$coefficients

vcov(m1)

m1 %>%
car::deltaMethod( "(Intercept)/x")
car::deltaMethod(m1, "(Intercept)/x")
car::deltaMethod(m1, "5")

car::deltaMethod(m1$coefficients, "(Intercept)/x", vcov = vcov(m1))

29.23229/-0.02869

msm::deltamethod(~ x1/x2, mean = c(29.23229, -0.02869),
                 cov = vcov(m1))

sqrt(emdbook::deltavar(`(Intercept)`/x,
                  meanval = m1$coefficients,
                  Sigma = vcov(m1)))
sqrt(emdbook::deltavar(5,
                       meanval = m1$coefficients,
                       Sigma = vcov(m1)))



g <- function(model) {
  coef(model)[1]/coef(model)[2]
}
marginaleffects::deltamethod(
  model = m1,
  FUN = g)
z <- function(model) {
  5
}
marginaleffects::deltamethod(
  model = m1,
  FUN = z)

pacman::p_load(gremlin)

m2 <- gremlin(y ~ x, data = bd)

gremlin::deltaSE(y ~ x, m2)

pacman::p_load(RMark)

RMark::deltamethod.special("sum", m1$coefficients, vcov(m1))

cov <- vcov(m1)
m1$coefficients %>%
  as_tibble_row() %>%
  rename(y = 1,
         x = 2) %>%
  summarise(tidydelta_m(~ y/x,
                        conf_lev = .95,
                        cov_dta = vcov(m1)))


boot::boot()
?boot


# Usual bootstrap of the ratio of means using the city data

coefs <- MASS::mvrnorm(n = 50, mu = coefficients(m1), Sigma = vcov(m1))

ratio <- function(`(Intercept)`, x) mean(`(Intercept)`)/mean(x)




# Example usage:
# Suppose you have a data frame 'df' with columns 'p' and 'delta'.
# If you want to compute the delta method for formula 'y ~ p^2 + delta^2', you can do:
# result_df <- delta_method(df, "p", "delta", formula = y ~ p^2 + delta^2)


library(remotes)
install_github("vincentarelbundock/marginaleffects")


library(marginaleffects)

yields <- rnorm(50, mean = 2000, sd = 10)
nitrogen <- rnorm(50, 110, sd = 5)
nitrogen_square <- nitrogen^2
Nitrogen_avg <- 110
Nitrogen_rate <- 0:110
reg <- lm(yields ~ nitrogen + nitrogen_square)

A <- marginaleffects::deltamethod(
  model = reg,
  hypothesis = "(nitrogen*Nitrogen_avg)+(nitrogen_square*Nitrogen_avg^2) = 0")

B <- marginaleffects::deltamethod(
  model = reg,
  hypothesis = "(nitrogen*Nitrogen_rate)+(nitrogen_square*Nitrogen_rate^2) = 0")

B$Marg_yield_diff <- A$estimate - B$estimate



pacman::p_load(msm)

set.seed(547)
bd <- tibble(x = rnorm(10000, mean = 5, sd = 245),
             y = rnorm(10000, mean = 35, sd = 343))


g=~log(x1^2) + 1/x2
mean <- c(mean(bd$x), mean(bd$y))
cov <- as.matrix(cov(bd %>%
                       select(x, y)))
ses = TRUE


cov <- as.matrix(cov)
n <- length(mean)
if (!is.list(g)) g <- list(g)
if ((dim(cov)[1] != n) || (dim(cov)[2] != n))
  stop(paste("Covariances should be a ", n, " by ", n,
             " matrix"))
syms <- paste("x", 1:n, sep = "")
for (i in 1:n) assign(syms[i], mean[i])
gdashmu <- t(sapply(g, function(form) {
  as.numeric(attr(eval(deriv(form, syms)), "gradient"))
}))
new.covar <- gdashmu %*% cov %*% t(gdashmu)
if (ses) {
  new.se <- sqrt(diag(new.covar))
  new.se
} else new.covar

grad_g
se <- as.vector(sqrt(t(grad_g) %*% cov %*% grad_g))





marginaleffects::deltamethod()



function (formula, data, subset, weights, na.action, method = "qr",
          model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
          contrasts = NULL, offset, ...)
{
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (method == "model.frame")
    return(mf)
  else if (method != "qr")
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
                     method), domain = NA)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
  offset <- model.offset(mf)
  mlm <- is.matrix(y)
  ny <- if (mlm)
    nrow(y)
  else length(y)
  if (!is.null(offset)) {
    if (!mlm)
      offset <- as.vector(offset)
    if (NROW(offset) != ny)
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                    NROW(offset), ny), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (mlm) matrix(NA_real_, 0,
                                             ncol(y)) else numeric(), residuals = y, fitted.values = 0 *
                y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
                                                                                0) else ny)
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    z <- if (is.null(w))
      lm.fit(x, y, offset = offset, singular.ok = singular.ok,
             ...)
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
                 ...)
  }
  class(z) <- c(if (mlm) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model)
    z$model <- mf
  if (ret.x)
    z$x <- x
  if (ret.y)
    z$y <- y
  if (!qr)
    z$qr <- NULL
  z
}
