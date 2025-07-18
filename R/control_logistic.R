#' Control function for logistic regression model fitting
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for logistic regression models.
#' `conf_level` refers to the confidence level used for the Odds Ratio CIs.
#'
#' @inheritParams argument_convention
#' @param response_definition (`string`)\cr the definition of what an event is in terms of `response`.
#'   This will be used when fitting the logistic regression model on the left hand side of the formula.
#'   Note that the evaluated expression should result in either a logical vector or a factor with 2
#'   levels. By default this is just `"response"` such that the original response variable is used
#'   and not modified further.
#'
#' @return A list of components with the same names as the arguments.
#'
#' @examples
#' # Standard options.
#' control_logistic()
#'
#' # Modify confidence level.
#' control_logistic(conf_level = 0.9)
#'
#' # Use a different response definition.
#' control_logistic(response_definition = "I(response %in% c('CR', 'PR'))")
#'
#' @export
control_logistic <- function(response_definition = "response",
                             conf_level = 0.95) {
  checkmate::assert_true(grepl("response", response_definition))
  checkmate::assert_string(response_definition)
  assert_proportion_value(conf_level)
  list(
    response_definition = response_definition,
    conf_level = conf_level
  )
}
