#' Helper functions for tabulating binary response by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions that tabulate in a data frame statistics such as response rate
#' and odds ratio for population subgroups.
#'
#' @inheritParams argument_convention
#' @inheritParams response_subgroups
#' @param arm (`factor`)\cr the treatment group variable.
#'
#' @details Main functionality is to prepare data for use in a layout-creating function.
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#'
#' adrs <- tern_ex_adrs
#' adrs_labels <- formatters::var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
#' @name h_response_subgroups
NULL

#' @describeIn h_response_subgroups Helper to prepare a data frame of binary responses by arm.
#'
#' @return
#' * `h_proportion_df()` returns a `data.frame` with columns `arm`, `n`, `n_rsp`, and `prop`.
#'
#' @examples
#' h_proportion_df(
#'   c(TRUE, FALSE, FALSE),
#'   arm = factor(c("A", "A", "B"), levels = c("A", "B"))
#' )
#'
#' @export
h_proportion_df <- function(rsp, arm) {
  checkmate::assert_logical(rsp)
  assert_valid_factor(arm, len = length(rsp))
  non_missing_rsp <- !is.na(rsp)
  rsp <- rsp[non_missing_rsp]
  arm <- arm[non_missing_rsp]

  lst_rsp <- split(rsp, arm)
  lst_results <- Map(function(x, arm) {
    if (length(x) > 0) {
      s_prop <- s_proportion(df = x)
      data.frame(
        arm = arm,
        n = length(x),
        n_rsp = unname(s_prop$n_prop[1]),
        prop = unname(s_prop$n_prop[2]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        arm = arm,
        n = 0L,
        n_rsp = NA,
        prop = NA,
        stringsAsFactors = FALSE
      )
    }
  }, lst_rsp, names(lst_rsp))

  df <- do.call(rbind, args = c(lst_results, make.row.names = FALSE))
  df$arm <- factor(df$arm, levels = levels(arm))
  df
}

#' @describeIn h_response_subgroups Summarizes proportion of binary responses by arm and across subgroups
#'    in a data frame. `variables` corresponds to the names of variables found in `data`, passed as a named list and
#'    requires elements `rsp`, `arm` and optionally `subgroups`. `groups_lists` optionally specifies
#'    groupings for `subgroups` variables.
#'
#' @return
#' * `h_proportion_subgroups_df()` returns a `data.frame` with columns `arm`, `n`, `n_rsp`, `prop`, `subgroup`,
#'   `var`, `var_label`, and `row_type`.
#'
#' @examples
#' h_proportion_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f
#' )
#'
#' # Define groupings for BMRKR2 levels.
#' h_proportion_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   )
#' )
#'
#' @export
h_proportion_subgroups_df <- function(variables,
                                      data,
                                      groups_lists = list(),
                                      label_all = "All Patients") {
  checkmate::assert_character(variables$rsp)
  checkmate::assert_character(variables$arm)
  checkmate::assert_character(variables$subgroups, null.ok = TRUE)
  assert_df_with_factors(data, list(val = variables$arm), min.levels = 2, max.levels = 2)
  assert_df_with_variables(data, variables)
  checkmate::assert_string(label_all)

  # Add All Patients.
  result_all <- h_proportion_df(data[[variables$rsp]], data[[variables$arm]])
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  # Add Subgroups.
  if (is.null(variables$subgroups)) {
    result_all
  } else {
    l_data <- h_split_by_subgroups(data, variables$subgroups, groups_lists = groups_lists)

    l_result <- lapply(l_data, function(grp) {
      result <- h_proportion_df(grp$df[[variables$rsp]], grp$df[[variables$arm]])
      result_labels <- grp$df_labels[rep(1, times = nrow(result)), ]
      cbind(result, result_labels)
    })
    result_subgroups <- do.call(rbind, args = c(l_result, make.row.names = FALSE))
    result_subgroups$row_type <- "analysis"

    rbind(
      result_all,
      result_subgroups
    )
  }
}

#' @describeIn h_response_subgroups Helper to prepare a data frame with estimates of
#'   the odds ratio between a treatment and a control arm.
#'
#' @inheritParams response_subgroups
#' @param strata_data (`factor`, `data.frame`, or `NULL`)\cr required if stratified analysis is performed.
#'
#' @return
#' * `h_odds_ratio_df()` returns a `data.frame` with columns `arm`, `n_tot`, `or`, `lcl`, `ucl`, `conf_level`, and
#'   optionally `pval` and `pval_label`.
#'
#' @examples
#' # Unstratatified analysis.
#' h_odds_ratio_df(
#'   c(TRUE, FALSE, FALSE, TRUE),
#'   arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
#' )
#'
#' # Include p-value.
#' h_odds_ratio_df(adrs_f$rsp, adrs_f$ARM, method = "chisq")
#'
#' # Stratatified analysis.
#' h_odds_ratio_df(
#'   rsp = adrs_f$rsp,
#'   arm = adrs_f$ARM,
#'   strata_data = adrs_f[, c("STRATA1", "STRATA2")],
#'   method = "cmh"
#' )
#'
#' @export
h_odds_ratio_df <- function(rsp, arm, strata_data = NULL, conf_level = 0.95, method = NULL) {
  assert_valid_factor(arm, n.levels = 2, len = length(rsp))

  df_rsp <- data.frame(
    rsp = rsp,
    arm = arm
  )

  if (!is.null(strata_data)) {
    strata_var <- interaction(strata_data, drop = TRUE)
    strata_name <- "strata"

    assert_valid_factor(strata_var, len = nrow(df_rsp))

    df_rsp[[strata_name]] <- strata_var
  } else {
    strata_name <- NULL
  }

  l_df <- split(df_rsp, arm)

  if (nrow(l_df[[1]]) > 0 && nrow(l_df[[2]]) > 0) {
    # Odds ratio and CI.
    result_odds_ratio <- s_odds_ratio(
      df = l_df[[2]],
      .var = "rsp",
      .ref_group = l_df[[1]],
      .in_ref_col = FALSE,
      .df_row = df_rsp,
      variables = list(arm = "arm", strata = strata_name),
      conf_level = conf_level
    )

    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = unname(result_odds_ratio$n_tot["n_tot"]),
      or = unname(result_odds_ratio$or_ci["est"]),
      lcl = unname(result_odds_ratio$or_ci["lcl"]),
      ucl = unname(result_odds_ratio$or_ci["ucl"]),
      conf_level = conf_level,
      stringsAsFactors = FALSE
    )

    if (!is.null(method)) {
      # Test for difference.
      result_test <- s_test_proportion_diff(
        df = l_df[[2]],
        .var = "rsp",
        .ref_group = l_df[[1]],
        .in_ref_col = FALSE,
        variables = list(strata = strata_name),
        method = method
      )

      df$pval <- as.numeric(result_test$pval)
      df$pval_label <- obj_label(result_test$pval)
    }

    # In those cases cannot go through the model so will obtain n_tot from data.
  } else if (
    (nrow(l_df[[1]]) == 0 && nrow(l_df[[2]]) > 0) ||
      (nrow(l_df[[1]]) > 0 && nrow(l_df[[2]]) == 0)
  ) {
    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = sum(stats::complete.cases(df_rsp)),
      or = NA,
      lcl = NA,
      ucl = NA,
      conf_level = conf_level,
      stringsAsFactors = FALSE
    )
    if (!is.null(method)) {
      df$pval <- NA
      df$pval_label <- NA
    }
  } else {
    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = 0L,
      or = NA,
      lcl = NA,
      ucl = NA,
      conf_level = conf_level,
      stringsAsFactors = FALSE
    )

    if (!is.null(method)) {
      df$pval <- NA
      df$pval_label <- NA
    }
  }

  df
}

#' @describeIn h_response_subgroups Summarizes estimates of the odds ratio between a treatment and a control
#'   arm across subgroups in a data frame. `variables` corresponds to the names of variables found in
#'   `data`, passed as a named list and requires elements `rsp`, `arm` and optionally `subgroups`
#'   and `strata`. `groups_lists` optionally specifies groupings for `subgroups` variables.
#'
#' @return
#' * `h_odds_ratio_subgroups_df()` returns a `data.frame` with columns `arm`, `n_tot`, `or`, `lcl`, `ucl`,
#'   `conf_level`, `subgroup`, `var`, `var_label`, and `row_type`.
#'
#' @examples
#' # Unstratified analysis.
#' h_odds_ratio_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f
#' )
#'
#' # Stratified analysis.
#' h_odds_ratio_subgroups_df(
#'   variables = list(
#'     rsp = "rsp",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2"),
#'     strata = c("STRATA1", "STRATA2")
#'   ),
#'   data = adrs_f
#' )
#'
#' # Define groupings of BMRKR2 levels.
#' h_odds_ratio_subgroups_df(
#'   variables = list(
#'     rsp = "rsp",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2")
#'   ),
#'   data = adrs_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   )
#' )
#'
#' @export
h_odds_ratio_subgroups_df <- function(variables,
                                      data,
                                      groups_lists = list(),
                                      conf_level = 0.95,
                                      method = NULL,
                                      label_all = "All Patients") {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `h_odds_ratio_subgroups_df() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

  checkmate::assert_character(variables$rsp)
  checkmate::assert_character(variables$arm)
  checkmate::assert_character(variables$subgroups, null.ok = TRUE)
  checkmate::assert_character(variables$strata, null.ok = TRUE)
  assert_df_with_factors(data, list(val = variables$arm), min.levels = 2, max.levels = 2)
  assert_df_with_variables(data, variables)
  checkmate::assert_string(label_all)

  strata_data <- if (is.null(variables$strata)) {
    NULL
  } else {
    data[, variables$strata, drop = FALSE]
  }

  # Add All Patients.
  result_all <- h_odds_ratio_df(
    rsp = data[[variables$rsp]],
    arm = data[[variables$arm]],
    strata_data = strata_data,
    conf_level = conf_level,
    method = method
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  if (is.null(variables$subgroups)) {
    result_all
  } else {
    l_data <- h_split_by_subgroups(data, variables$subgroups, groups_lists = groups_lists)

    l_result <- lapply(l_data, function(grp) {
      grp_strata_data <- if (is.null(variables$strata)) {
        NULL
      } else {
        grp$df[, variables$strata, drop = FALSE]
      }

      result <- h_odds_ratio_df(
        rsp = grp$df[[variables$rsp]],
        arm = grp$df[[variables$arm]],
        strata_data = grp_strata_data,
        conf_level = conf_level,
        method = method
      )
      result_labels <- grp$df_labels[rep(1, times = nrow(result)), ]
      cbind(result, result_labels)
    })

    result_subgroups <- do.call(rbind, args = c(l_result, make.row.names = FALSE))
    result_subgroups$row_type <- "analysis"

    rbind(
      result_all,
      result_subgroups
    )
  }
}
