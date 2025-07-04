#' Create a STEP graph
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Based on the STEP results, creates a `ggplot` graph showing the estimated HR or OR
#' along the continuous biomarker value subgroups.
#'
#' @param df (`tibble`)\cr result of [tidy.step()].
#' @param use_percentile (`flag`)\cr whether to use percentiles for the x axis or actual
#'   biomarker values.
#' @param est (named `list`)\cr `col` and `lty` settings for estimate line.
#' @param ci_ribbon (named `list` or `NULL`)\cr `fill` and `alpha` settings for the confidence interval
#'   ribbon area, or `NULL` to not plot a CI ribbon.
#' @param col (`character`)\cr color(s).
#'
#' @return A `ggplot` STEP graph.
#'
#' @seealso Custom tidy method [tidy.step()].
#'
#' @examples
#' library(survival)
#' lung$sex <- factor(lung$sex)
#'
#' # Survival example.
#' vars <- list(
#'   time = "time",
#'   event = "status",
#'   arm = "sex",
#'   biomarker = "age"
#' )
#'
#' step_matrix <- fit_survival_step(
#'   variables = vars,
#'   data = lung,
#'   control = c(control_coxph(), control_step(num_points = 10, degree = 2))
#' )
#' step_data <- broom::tidy(step_matrix)
#'
#' # Default plot.
#' g_step(step_data)
#'
#' # Add the reference 1 horizontal line.
#' library(ggplot2)
#' g_step(step_data) +
#'   ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = 2)
#'
#' # Use actual values instead of percentiles, different color for estimate and no CI,
#' # use log scale for y axis.
#' g_step(
#'   step_data,
#'   use_percentile = FALSE,
#'   est = list(col = "blue", lty = 1),
#'   ci_ribbon = NULL
#' ) + scale_y_log10()
#'
#' # Adding another curve based on additional column.
#' step_data$extra <- exp(step_data$`Percentile Center`)
#' g_step(step_data) +
#'   ggplot2::geom_line(ggplot2::aes(y = extra), linetype = 2, color = "green")
#'
#' # Response example.
#' vars <- list(
#'   response = "status",
#'   arm = "sex",
#'   biomarker = "age"
#' )
#'
#' step_matrix <- fit_rsp_step(
#'   variables = vars,
#'   data = lung,
#'   control = c(
#'     control_logistic(response_definition = "I(response == 2)"),
#'     control_step()
#'   )
#' )
#' step_data <- broom::tidy(step_matrix)
#' g_step(step_data)
#'
#' @export
g_step <- function(df,
                   use_percentile = "Percentile Center" %in% names(df),
                   est = list(col = "blue", lty = 1),
                   ci_ribbon = list(fill = getOption("ggplot2.discrete.colour")[1], alpha = 0.5),
                   col = getOption("ggplot2.discrete.colour")) {
  checkmate::assert_tibble(df)
  checkmate::assert_flag(use_percentile)
  checkmate::assert_character(col, null.ok = TRUE)
  checkmate::assert_list(est, names = "named")
  checkmate::assert_list(ci_ribbon, names = "named", null.ok = TRUE)

  x_var <- ifelse(use_percentile, "Percentile Center", "Interval Center")
  df$x <- df[[x_var]]
  attrs <- attributes(df)
  df$y <- df[[attrs$estimate]]

  # Set legend names. To be modified also at call level
  legend_names <- c("Estimate", "CI 95%")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]))

  if (!is.null(col)) {
    p <- p +
      ggplot2::scale_color_manual(values = col)
  }

  if (!is.null(ci_ribbon)) {
    if (is.null(ci_ribbon$fill)) {
      ci_ribbon$fill <- "lightblue"
    }
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data[["ci_lower"]], ymax = .data[["ci_upper"]],
        fill = legend_names[2]
      ),
      alpha = ci_ribbon$alpha
    ) +
      scale_fill_manual(
        name = "", values = c("CI 95%" = ci_ribbon$fill)
      )
  }
  suppressMessages(p <- p +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["y"]], color = legend_names[1]),
      linetype = est$lty
    ) +
    scale_colour_manual(
      name = "", values = c("Estimate" = "blue")
    ))

  p <- p + ggplot2::labs(x = attrs$biomarker, y = attrs$estimate)
  if (use_percentile) {
    p <- p + ggplot2::scale_x_continuous(labels = scales::percent)
  }
  p
}

#' Custom tidy method for STEP results
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Tidy the STEP results into a `tibble` format ready for plotting.
#'
#' @param x (`matrix`)\cr results from [fit_survival_step()].
#' @param ... not used.
#'
#' @return A `tibble` with one row per STEP subgroup. The estimates and CIs are on the HR or OR scale,
#'   respectively. Additional attributes carry metadata also used for plotting.
#'
#' @seealso [g_step()] which consumes the result from this function.
#'
#' @method tidy step
#'
#' @examples
#' library(survival)
#' lung$sex <- factor(lung$sex)
#' vars <- list(
#'   time = "time",
#'   event = "status",
#'   arm = "sex",
#'   biomarker = "age"
#' )
#' step_matrix <- fit_survival_step(
#'   variables = vars,
#'   data = lung,
#'   control = c(control_coxph(), control_step(num_points = 10, degree = 2))
#' )
#' broom::tidy(step_matrix)
#'
#' @export
tidy.step <- function(x, ...) { # nolint
  checkmate::assert_class(x, "step")
  dat <- as.data.frame(x)
  nams <- names(dat)
  is_surv <- "loghr" %in% names(dat)
  est_var <- ifelse(is_surv, "loghr", "logor")
  new_est_var <- ifelse(is_surv, "Hazard Ratio", "Odds Ratio")
  new_y_vars <- c(new_est_var, c("ci_lower", "ci_upper"))
  names(dat)[match(est_var, nams)] <- new_est_var
  dat[, new_y_vars] <- exp(dat[, new_y_vars])
  any_is_na <- any(is.na(dat[, new_y_vars]))
  any_is_very_large <- any(abs(dat[, new_y_vars]) > 1e10, na.rm = TRUE)
  if (any_is_na) {
    warning(paste(
      "Missing values in the point estimate or CI columns,",
      "this will lead to holes in the `g_step()` plot"
    ))
  }
  if (any_is_very_large) {
    warning(paste(
      "Very large absolute values in the point estimate or CI columns,",
      "consider adding `scale_y_log10()` to the `g_step()` result for plotting"
    ))
  }
  if (any_is_na || any_is_very_large) {
    warning("Consider using larger `bandwidth`, less `num_points` in `control_step()` settings for fitting")
  }
  structure(
    tibble::as_tibble(dat),
    estimate = new_est_var,
    biomarker = attr(x, "variables")$biomarker,
    ci = f_conf_level(attr(x, "control")$conf_level)
  )
}
