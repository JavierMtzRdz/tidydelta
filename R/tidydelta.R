#' Convert a formula to an expression
#'
#' Converts a formula to an expression for further evaluation.
#'
#' @param formula A formula object or a character string representing a formula.
#'
#' @return The evaluated expression.
#'
for_to_exp <- function(formula) {
  if (rlang::is_formula(formula)) {
    formula <- rlang::as_label(formula)
  }
  eval(parse(text = paste0("rlang::expr(", gsub("~", "", formula), ")")))
}


#' Extract variables from a formula
#'
#' Extracts variables from a formula string.
#'
#' @param formula A formula object or a character string representing a formula.
#'
#' @return A named character vector of extracted variables.
#'
ext_bd_var <- function(formula) {
  if (!rlang::is_formula(formula)) {
    formula <- stats::as.formula(formula)
  }

  vars_select <- all.vars(formula)
  form_lab <- rlang::as_label(formula)

  # Define a regular expression pattern to match variables
  functs_collap <- paste(vars_select, collapse = "|")
  functs_name <- paste0("\\$(", functs_collap, ")")
  functs <- paste0("(", functs_collap, ")\\$", "(", functs_collap, ")")

  # Use regexpr and regmatches to extract matches
  ext_vars <- regmatches(
    form_lab,
    gregexpr(
      functs,
      form_lab
    )
  )[[1]]

  # Remove the $ character and set names
  names(ext_vars) <- gsub(
    "\\$", "",
    regmatches(
      form_lab,
      gregexpr(
        functs_name,
        form_lab
      )
    )[[1]]
  )

  return(ext_vars)
}

#' Recursive search of environment
#'
#' Recursive search of environment containing object.
#'
#' @param name Object searched
#' @param env Initial environment to search
#'
#' @return A named character vector of extracted variables.
#'

where_env <- function(name, env = rlang::caller_env()) {
  envs <- rlang::search_envs()

  i <- 1

  env <- envs[[i]]

  while (!exists(name[1],
           envir = env
         )) {
    i <- i + 1

    if (i > length(envs)) cli::cli_abort("Object not found in envs.")

    env <- envs[[i]]
  }

  return(env)
}

#' Extract variables and their names from the formula
#'
#' Extract variables and their names from the formula
#'
#' @param formula A formula object specifying the variables of interest.
#' @param mean_dta Vector containing the means of the variables.
#' @param cov_dta Covariance matrix of the variables.
#'
#' @return list containing objects with variables and formula
cases_ext <- function(formula,
                      mean_dta = NULL,
                      cov_dta = NULL) {
  vars_names <- all.names(formula)
  vars_select <- all.vars(formula)
  form_lab <- rlang::as_label(formula)
  vars <- tibble::tibble()

  if (length(vars_select) == 0) {
    cli::cli_abort("The function does not meet the differentiability criteria.")
  }

  if (!is.null(mean_dta) && is.null(cov_dta)) {
    cli::cli_abort("You need to add the covariance matrix in 'cov_dta'.")
  }

  if (any(vars_names == "$") && is.null(mean_dta)) {
    # Extract variables with "$" notation
    vars_bd <- ext_bd_var(form_lab)

    vars <- tibble::as_tibble(
      lapply(vars_bd, function(x) {
        eval(parse(text = x))
      })
    )

    inv_vars_bd <- names(vars_bd)
    names(inv_vars_bd) <- as.character(vars_bd)

    fun_exp <- for_to_exp(gsub(
      paste(paste0(vars_select, "\\$"),
        collapse = "|"
      ), "",
      form_lab
    ))
  }
  if (all(vars_names != "$") && is.null(mean_dta)) {
    env_pl <- where_env(vars_select)
    vars <- tibble::as_tibble(
      mget(vars_select,
        inherits = TRUE,
        envir = env_pl
      )
    )


    fun_exp <- for_to_exp(formula)
  }

  if (!is.null(mean_dta)) {
    # Extract variables
    vars <- tibble::as_tibble(mean_dta)

    fun_exp <- for_to_exp(formula)
  }

  return(list("vars" = vars, "fun_exp" = fun_exp))
}


#' Delta Method implementation
#'
#' Estimates standard errors for transformations of random variables
#' using Delta method.
#'
#' @param formula A formula object specifying the variables of interest.
#' @param normality_eval Logical value to run normality test in case of being
#' possible.
#' @param formula_vars The function(s) to apply to the variables in the formula.
#' @param mean_dta Vector containing the means of the variables.
#' @param cov_dta Covariance matrix of the variables.
#' @param conf_lev Confidence level for confidence intervals.
#' @param n Sample size evaluation (in case that we can evaluate the confidence
#' intervals with different hypnotic sample sizes).
#'
#' @return A tibble with columns for means, standard errors, and optionally,
#'  confidence intervals.
#'
#' @examples
#' # Equivalent ways to use tidydelta()
#' library(tidyverse)
#'
#' x <- rnorm(1000, mean = 5, sd = 2)
#' y <- rnorm(1000, mean = 15, sd = 3)
#'
#' bd <- tibble(x, y)
#'
#' tidydelta(~ y / x,
#'   conf_lev = .95
#' )
#'
#' tidydelta(~ bd$y / bd$x,
#'   conf_lev = .95
#' )
#' bd %>%
#'   summarise(tidydelta(~ y / x,
#'     conf_lev = .95
#'   ))
#'
#' @export
tidydelta <- function(
    formula,
    normality_eval = TRUE,
    formula_vars = mean,
    mean_dta = NULL,
    cov_dta = NULL,
    n = NULL,
    conf_lev = .95) {
  exp_for <- cases_ext(
    formula,
    mean_dta,
    cov_dta
  )

  vars <- exp_for$vars

  fun_exp <- exp_for$fun_exp

  # n dta ---
  n_data <- dim(vars)[1]
  # Evaluate normality
  if (n_data > 30 && normality_eval) {
    vars_norm <- if (n_data > 5000) {
      dplyr::sample_n(vars, 5000)
    } else {
      vars
    }

    tests <- sapply(names(vars_norm), function(x) {
      stats::shapiro.test(purrr::as_vector(vars_norm[, x]))
    })

    tests_pv <- tests["p.value", ]

    if (any(tests_pv < 0.1)) {
      cli::cli_warn(
        "The provided variables do not meet the normality criteria."
      )
    }
  }

  # Calculate the covariance matrix if not provided
  if (is.null(cov_dta)) {
    cov_est <- if (dim(vars)[2] == 1) stats::var(vars) else stats::cov(vars)
  } else {
    cov_est <- cov_dta
  }

  # n estimation ---
  if (is.null(n)) {
    n <- 1
  }

  # Calculate the means of selected variables
  var_mean <- dplyr::summarise(
    vars,
    dplyr::across(
      dplyr::where(is.numeric),
      formula_vars
    )
  )

  # Convert var_mean to a vector
  var_mean_vec <- purrr::as_vector(var_mean)

  # Define a function for transformation
  f <- function(z, fun = fun_exp) {
    quo <- rlang::quo_squash(fun)
    rlang::eval_tidy(quo, z)
  }

  transformation <- f(var_mean)

  # Evaluate the derivative of f

  g_eva <- numDeriv::jacobian(
    func = f, var_mean_vec,
    method = "Richardson",
    side = rep(-1, length(var_mean_vec))
  )

  # Check differentiability
  if (any(is.na(g_eva)) &&
        all(!is.nan(g_eva))) {
    cli::cli_abort("The function does not meet the differentiability criteria.")
  }
  if (all(g_eva == 0)) {
    cli::cli_abort("The function does not meet the differentiability criteria.")
  }

  # Calculate the gradient of f
  grad_g <- t(
    numDeriv::jacobian(
      func = f,
      var_mean_vec
    )
  )

  # Estimate the standard error
  se_res <- as.vector(
    sqrt(t(grad_g) %*% cov_est %*% grad_g) / sqrt(n)
  )

  # Create the result tibble
  ret_tib <- dplyr::bind_cols(
    var_mean,
    tibble::tibble(
      `T_n` = transformation,
      se = se_res
    )
  )

  # Optionally, calculate and add confidence intervals
  if (!is.null(conf_lev)) {
    z <- stats::qnorm(conf_lev + (1 - conf_lev) / 2)
    ret_tib <- dplyr::bind_cols(
      ret_tib,
      tibble::tibble(
        lower_ci = transformation - z * se_res,
        upper_ci = transformation + z * se_res
      )
    )
  }

  return(ret_tib)
}
