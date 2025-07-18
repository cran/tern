#' Control function for Cox-PH model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for Cox-PH model, typically used internally to specify
#' details of Cox-PH model for [s_coxph_pairwise()]. `conf_level` refers to Hazard Ratio estimation.
#'
#' @inheritParams argument_convention
#' @param pval_method (`string`)\cr p-value method for testing hazard ratio = 1.
#'   Default method is `"log-rank"`, can also be set to `"wald"` or `"likelihood"`.
#' @param ties (`string`)\cr string specifying the method for tie handling. Default is `"efron"`,
#'   can also be set to `"breslow"` or `"exact"`. See more in [survival::coxph()].
#'
#' @return A list of components with the same names as the arguments.
#'
#' @export
control_coxph <- function(pval_method = c("log-rank", "wald", "likelihood"),
                          ties = c("efron", "breslow", "exact"),
                          conf_level = 0.95) {
  pval_method <- match.arg(pval_method)
  ties <- match.arg(ties)
  assert_proportion_value(conf_level)

  list(pval_method = pval_method, ties = ties, conf_level = conf_level)
}

#' Control function for `survfit` models for survival time
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for `survfit` model, typically used internally to specify
#' details of `survfit` model for [s_surv_time()]. `conf_level` refers to survival time estimation.
#'
#' @inheritParams argument_convention
#' @param conf_type (`string`)\cr confidence interval type. Options are "plain" (default), "log", "log-log",
#'   see more in [survival::survfit()]. Note option "none" is no longer supported.
#' @param quantiles (`numeric(2)`)\cr vector of length two specifying the quantiles of survival time.
#'
#' @return A list of components with the same names as the arguments.
#'
#' @export
control_surv_time <- function(conf_level = 0.95,
                              conf_type = c("plain", "log", "log-log"),
                              quantiles = c(0.25, 0.75)) {
  conf_type <- match.arg(conf_type)
  checkmate::assert_numeric(quantiles, lower = 0, upper = 1, len = 2, unique = TRUE, sorted = TRUE)
  nullo <- lapply(quantiles, assert_proportion_value)
  assert_proportion_value(conf_level)
  list(conf_level = conf_level, conf_type = conf_type, quantiles = quantiles)
}

#' Control function for `survfit` models for patients' survival rate at time points
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for `survfit` model, typically used internally to specify
#' details of `survfit` model for [s_surv_timepoint()]. `conf_level` refers to patient risk estimation at a time point.
#'
#' @inheritParams argument_convention
#' @inheritParams control_surv_time
#'
#' @return A list of components with the same names as the arguments.
#'
#' @export
control_surv_timepoint <- function(conf_level = 0.95,
                                   conf_type = c("plain", "log", "log-log")) {
  conf_type <- match.arg(conf_type)
  assert_proportion_value(conf_level)
  list(
    conf_level = conf_level,
    conf_type = conf_type
  )
}
