#' Odds ratio estimation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [estimate_odds_ratio()] creates a layout element to compare bivariate responses between
#' two groups by estimating an odds ratio and its confidence interval.
#'
#' The primary analysis variable specified by `vars` is the group variable. Additional variables can be included in the
#' analysis via the `variables` argument, which accepts `arm`, an arm variable, and `strata`, a stratification variable.
#' If more than two arm levels are present, they can be combined into two groups using the `groups_list` argument.
#'
#' @inheritParams split_cols_by_groups
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("estimate_odds_ratio"), type = "sh")``
#' @param method (`string`)\cr whether to use the correct (`"exact"`) calculation in the conditional likelihood or one
#'   of the approximations. See [survival::clogit()] for details.
#'
#' @note
#' * This function uses logistic regression for unstratified analyses, and conditional logistic regression for
#'   stratified analyses. The Wald confidence interval is calculated with the specified confidence level.
#' * For stratified analyses, there is currently no implementation for conditional likelihood confidence intervals,
#'   therefore the likelihood confidence interval is not available as an option.
#' * When `vars` contains only responders or non-responders no odds ratio estimation is possible so the returned
#'   values will be `NA`.
#'
#' @seealso Relevant helper function [h_odds_ratio()].
#'
#' @name odds_ratio
#' @order 1
NULL

#' @describeIn odds_ratio Statistics function which estimates the odds ratio
#'   between a treatment and a control. A `variables` list with `arm` and `strata`
#'   variable names must be passed if a stratified analysis is required.
#'
#' @return
#' * `s_odds_ratio()` returns a named list with the statistics `or_ci`
#'   (containing `est`, `lcl`, and `ucl`) and `n_tot`.
#'
#' @examples
#' # Unstratified analysis.
#' s_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta
#' )
#'
#' # Stratified analysis.
#' s_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta,
#'   variables = list(arm = "grp", strata = "strata")
#' )
#'
#' @export
s_odds_ratio <- function(df,
                         .var,
                         .ref_group,
                         .in_ref_col,
                         .df_row,
                         variables = list(arm = NULL, strata = NULL),
                         conf_level = 0.95,
                         groups_list = NULL,
                         method = "exact",
                         ...) {
  y <- list(or_ci = numeric(), n_tot = numeric())

  if (!.in_ref_col) {
    assert_proportion_value(conf_level)
    assert_df_with_variables(df, list(rsp = .var))
    assert_df_with_variables(.ref_group, list(rsp = .var))

    if (is.null(variables$strata)) {
      data <- data.frame(
        rsp = c(.ref_group[[.var]], df[[.var]]),
        grp = factor(
          rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))),
          levels = c("ref", "Not-ref")
        )
      )
      y <- or_glm(data, conf_level = conf_level)
    } else {
      assert_df_with_variables(.df_row, c(list(rsp = .var), variables))
      checkmate::assert_subset(method, c("exact", "approximate", "efron", "breslow"), empty.ok = FALSE)

      # The group variable prepared for clogit must be synchronised with combination groups definition.
      if (is.null(groups_list)) {
        ref_grp <- as.character(unique(.ref_group[[variables$arm]]))
        trt_grp <- as.character(unique(df[[variables$arm]]))
        grp <- stats::relevel(factor(.df_row[[variables$arm]]), ref = ref_grp)
      } else {
        # If more than one level in reference col.
        reference <- as.character(unique(.ref_group[[variables$arm]]))
        grp_ref_flag <- vapply(
          X = groups_list,
          FUN.VALUE = TRUE,
          FUN = function(x) all(reference %in% x)
        )
        ref_grp <- names(groups_list)[grp_ref_flag]

        # If more than one level in treatment col.
        treatment <- as.character(unique(df[[variables$arm]]))
        grp_trt_flag <- vapply(
          X = groups_list,
          FUN.VALUE = TRUE,
          FUN = function(x) all(treatment %in% x)
        )
        trt_grp <- names(groups_list)[grp_trt_flag]

        grp <- combine_levels(.df_row[[variables$arm]], levels = reference, new_level = ref_grp)
        grp <- combine_levels(grp, levels = treatment, new_level = trt_grp)
      }

      # The reference level in `grp` must be the same as in the `rtables` column split.
      data <- data.frame(
        rsp = .df_row[[.var]],
        grp = grp,
        strata = interaction(.df_row[variables$strata])
      )
      y_all <- or_clogit(data, conf_level = conf_level, method = method)
      checkmate::assert_string(trt_grp)
      checkmate::assert_subset(trt_grp, names(y_all$or_ci))
      y$or_ci <- y_all$or_ci[[trt_grp]]
      y$n_tot <- y_all$n_tot
    }
  }

  if ("est" %in% names(y$or_ci) && is.na(y$or_ci[["est"]]) && method != "approximate") {
    warning(
      "Unable to compute the odds ratio estimate. Please try re-running the function with ",
      'parameter `method` set to "approximate".'
    )
  }

  y$or_ci <- formatters::with_label(
    x = y$or_ci,
    label = paste0("Odds Ratio (", 100 * conf_level, "% CI)")
  )

  y$n_tot <- formatters::with_label(
    x = y$n_tot,
    label = "Total n"
  )

  y
}

#' @describeIn odds_ratio Formatted analysis function which is used as `afun` in `estimate_odds_ratio()`.
#'
#' @return
#' * `a_odds_ratio()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_odds_ratio(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta
#' )
#'
#' @export
a_odds_ratio <- function(df,
                         ...,
                         .stats = NULL,
                         .stat_names = NULL,
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  # Check for user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_odds_ratio,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("estimate_odds_ratio",
    stats_in = .stats,
    custom_stats_in = names(custom_stat_functions)
  )
  x_stats <- x_stats[.stats]
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(
    .stats, .labels,
    tern_defaults = c(lapply(x_stats, attr, "label"), tern_default_labels)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn odds_ratio Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `estimate_odds_ratio()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_odds_ratio()` to the table layout.
#'
#' @examples
#' set.seed(12)
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
#'   strata = factor(sample(c("C", "D"), 100, TRUE))
#' )
#'
#' l <- basic_table() %>%
#'   split_cols_by(var = "grp", ref_group = "B") %>%
#'   estimate_odds_ratio(vars = "rsp")
#'
#' build_table(l, df = dta)
#'
#' @export
#' @order 2
estimate_odds_ratio <- function(lyt,
                                vars,
                                variables = list(arm = NULL, strata = NULL),
                                conf_level = 0.95,
                                groups_list = NULL,
                                method = "exact",
                                na_str = default_na_str(),
                                nested = TRUE,
                                ...,
                                table_names = vars,
                                show_labels = "hidden",
                                var_labels = vars,
                                .stats = "or_ci",
                                .stat_names = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    variables = list(variables), conf_level = list(conf_level), groups_list = list(groups_list), method = list(method),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_odds_ratio) <- c(formals(a_odds_ratio), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = vars,
    afun = a_odds_ratio,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}

#' Helper functions for odds ratio estimation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions to calculate odds ratios in [estimate_odds_ratio()].
#'
#' @inheritParams odds_ratio
#' @inheritParams argument_convention
#' @param data (`data.frame`)\cr data frame containing at least the variables `rsp` and `grp`, and optionally
#'   `strata` for [or_clogit()].
#'
#' @return A named `list` of elements `or_ci` and `n_tot`.
#'
#' @seealso [odds_ratio]
#'
#' @name h_odds_ratio
NULL

#' @describeIn h_odds_ratio Estimates the odds ratio based on [stats::glm()]. Note that there must be
#'   exactly 2 groups in `data` as specified by the `grp` variable.
#'
#' @examples
#' # Data with 2 groups.
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
#'   strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' # Odds ratio based on glm.
#' or_glm(data, conf_level = 0.95)
#'
#' @export
or_glm <- function(data, conf_level) {
  checkmate::assert_logical(data$rsp)
  assert_proportion_value(conf_level)
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp"))
  checkmate::assert_multi_class(data$grp, classes = c("factor", "character"))

  data$grp <- as_factor_keep_attributes(data$grp)
  assert_df_with_factors(data, list(val = "grp"), min.levels = 2, max.levels = 2)
  formula <- stats::as.formula("rsp ~ grp")
  model_fit <- stats::glm(
    formula = formula, data = data,
    family = stats::binomial(link = "logit")
  )

  # Note that here we need to discard the intercept.
  or <- exp(stats::coef(model_fit)[-1])
  or_ci <- exp(
    stats::confint.default(model_fit, level = conf_level)[-1, , drop = FALSE]
  )

  values <- stats::setNames(c(or, or_ci), c("est", "lcl", "ucl"))
  n_tot <- stats::setNames(nrow(model_fit$model), "n_tot")

  list(or_ci = values, n_tot = n_tot)
}

#' @describeIn h_odds_ratio Estimates the odds ratio based on [survival::clogit()]. This is done for
#'   the whole data set including all groups, since the results are not the same as when doing
#'   pairwise comparisons between the groups.
#'
#' @examples
#' # Data with 3 groups.
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
#'   strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' # Odds ratio based on stratified estimation by conditional logistic regression.
#' or_clogit(data, conf_level = 0.95)
#'
#' @export
or_clogit <- function(data, conf_level, method = "exact") {
  checkmate::assert_logical(data$rsp)
  assert_proportion_value(conf_level)
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp", strata = "strata"))
  checkmate::assert_multi_class(data$grp, classes = c("factor", "character"))
  checkmate::assert_multi_class(data$strata, classes = c("factor", "character"))
  checkmate::assert_subset(method, c("exact", "approximate", "efron", "breslow"), empty.ok = FALSE)

  data$grp <- as_factor_keep_attributes(data$grp)
  data$strata <- as_factor_keep_attributes(data$strata)

  # Deviation from convention: `survival::strata` must be simply `strata`.
  formula <- stats::as.formula("rsp ~ grp + strata(strata)")
  model_fit <- clogit_with_tryCatch(formula = formula, data = data, method = method)

  # Create a list with one set of OR estimates and CI per coefficient, i.e.
  # comparison of one group vs. the reference group.
  coef_est <- stats::coef(model_fit)
  ci_est <- stats::confint(model_fit, level = conf_level)
  or_ci <- list()
  for (coef_name in names(coef_est)) {
    grp_name <- gsub("^grp", "", x = coef_name)
    or_ci[[grp_name]] <- stats::setNames(
      object = exp(c(coef_est[coef_name], ci_est[coef_name, , drop = TRUE])),
      nm = c("est", "lcl", "ucl")
    )
  }
  list(or_ci = or_ci, n_tot = c(n_tot = model_fit$n))
}
