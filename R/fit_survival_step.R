#' Subgroup treatment effect pattern (STEP) fit for survival outcome
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This fits the subgroup treatment effect pattern (STEP) models for a survival outcome. The treatment arm
#' variable must have exactly 2 levels, where the first one is taken as reference and the estimated
#' hazard ratios are for the comparison of the second level vs. the first one.
#'
#' The model which is fit is:
#'
#' `Surv(time, event) ~ arm * poly(biomarker, degree) + covariates + strata(strata)`
#'
#' where `degree` is specified by `control_step()`.
#'
#' @inheritParams argument_convention
#' @param variables (named `list` of `character`)\cr list of analysis variables: needs `time`, `event`,
#'   `arm`, `biomarker`, and optional `covariates` and `strata`.
#' @param control (named `list`)\cr combined control list from [control_step()] and [control_coxph()].
#'
#' @return A matrix of class `step`. The first part of the columns describe the subgroup intervals used
#'   for the biomarker variable, including where the center of the intervals are and their bounds. The
#'   second part of the columns contain the estimates for the treatment arm comparison.
#'
#' @note For the default degree 0 the `biomarker` variable is not included in the model.
#'
#' @seealso [control_step()] and [control_coxph()] for the available customization options.
#'
#' @examples
#' # Testing dataset with just two treatment arms.
#' library(dplyr)
#'
#' adtte_f <- tern_ex_adtte %>%
#'   filter(
#'     PARAMCD == "OS",
#'     ARM %in% c("B: Placebo", "A: Drug X")
#'   ) %>%
#'   mutate(
#'     # Reorder levels of ARM to display reference arm before treatment arm.
#'     ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
#'     is_event = CNSR == 0
#'   )
#' labels <- c("ARM" = "Treatment Arm", "is_event" = "Event Flag")
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
#'
#' variables <- list(
#'   arm = "ARM",
#'   biomarker = "BMRKR1",
#'   covariates = c("AGE", "BMRKR2"),
#'   event = "is_event",
#'   time = "AVAL"
#' )
#'
#' # Fit default STEP models: Here a constant treatment effect is estimated in each subgroup.
#' step_matrix <- fit_survival_step(
#'   variables = variables,
#'   data = adtte_f
#' )
#' dim(step_matrix)
#' head(step_matrix)
#'
#' # Specify different polynomial degree for the biomarker interaction to use more flexible local
#' # models. Or specify different Cox regression options.
#' step_matrix2 <- fit_survival_step(
#'   variables = variables,
#'   data = adtte_f,
#'   control = c(control_coxph(conf_level = 0.9), control_step(degree = 2))
#' )
#'
#' # Use a global model with cubic interaction and only 5 points.
#' step_matrix3 <- fit_survival_step(
#'   variables = variables,
#'   data = adtte_f,
#'   control = c(control_coxph(), control_step(bandwidth = NULL, degree = 3, num_points = 5L))
#' )
#'
#' @export
fit_survival_step <- function(variables,
                              data,
                              control = c(control_step(), control_coxph())) {
  checkmate::assert_list(control)
  assert_df_with_variables(data, variables)
  data <- data[!is.na(data[[variables$biomarker]]), ]
  window_sel <- h_step_window(x = data[[variables$biomarker]], control = control)
  interval_center <- window_sel$interval[, "Interval Center"]
  form <- h_step_survival_formula(variables = variables, control = control)
  estimates <- if (is.null(control$bandwidth)) {
    h_step_survival_est(
      formula = form,
      data = data,
      variables = variables,
      x = interval_center,
      control = control
    )
  } else {
    tmp <- mapply(
      FUN = h_step_survival_est,
      x = interval_center,
      subset = as.list(as.data.frame(window_sel$sel)),
      MoreArgs = list(
        formula = form,
        data = data,
        variables = variables,
        control = control
      )
    )
    # Maybe we find a more elegant solution than this.
    rownames(tmp) <- c("n", "events", "loghr", "se", "ci_lower", "ci_upper")
    t(tmp)
  }
  result <- cbind(window_sel$interval, estimates)
  structure(
    result,
    class = c("step", "matrix"),
    variables = variables,
    control = control
  )
}
