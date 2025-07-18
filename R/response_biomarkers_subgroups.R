#' Tabulate biomarker effects on binary response by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [tabulate_rsp_biomarkers()] function creates a layout element to tabulate the estimated biomarker effects on a
#' binary response endpoint across subgroups, returning statistics including response rate and odds ratio for each
#' population subgroup. The table is created from `df`, a list of data frames returned by [extract_rsp_biomarkers()],
#' with the statistics to include specified via the `vars` parameter.
#'
#' A forest plot can be created from the resulting table using the [g_forest()] function.
#'
#' @inheritParams argument_convention
#' @param df (`data.frame`)\cr containing all analysis variables, as returned by
#'   [extract_rsp_biomarkers()].
#' @param vars (`character`)\cr the names of statistics to be reported among:
#'   * `n_tot`: Total number of patients per group.
#'   * `n_rsp`: Total number of responses per group.
#'   * `prop`: Total response proportion per group.
#'   * `or`: Odds ratio.
#'   * `ci`: Confidence interval of odds ratio.
#'   * `pval`: p-value of the effect.
#'   Note, the statistics `n_tot`, `or` and `ci` are required.
#'
#' @return An `rtables` table summarizing biomarker effects on binary response by subgroup.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. The tables are then typically used as input for forest plots.
#'
#' @note In contrast to [tabulate_rsp_subgroups()] this tabulation function does
#'   not start from an input layout `lyt`. This is because internally the table is
#'   created by combining multiple subtables.
#'
#' @seealso [extract_rsp_biomarkers()]
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
#'   mutate(rsp = AVALC == "CR")
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
#' df <- extract_rsp_biomarkers(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   data = adrs_f
#' )
#'
#' \donttest{
#' ## Table with default columns.
#' tabulate_rsp_biomarkers(df)
#'
#' ## Table with a manually chosen set of columns: leave out "pval", reorder.
#' tab <- tabulate_rsp_biomarkers(
#'   df = df,
#'   vars = c("n_rsp", "ci", "n_tot", "prop", "or")
#' )
#'
#' ## Finally produce the forest plot.
#' g_forest(tab, xlim = c(0.7, 1.4))
#' }
#'
#' @export
#' @name response_biomarkers_subgroups
tabulate_rsp_biomarkers <- function(df,
                                    vars = c("n_tot", "n_rsp", "prop", "or", "ci", "pval"),
                                    na_str = default_na_str(),
                                    ...,
                                    .stat_names = NULL,
                                    .formats = NULL,
                                    .labels = NULL,
                                    .indent_mods = NULL) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(df$biomarker)
  checkmate::assert_character(df$biomarker_label)
  checkmate::assert_subset(vars, get_stats("tabulate_rsp_biomarkers"))

  # Process standard extra arguments
  extra_args <- list(".stats" = vars)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  colvars <- d_rsp_subgroups_colvars(
    vars,
    conf_level = df$conf_level[1],
    method = df$pval_label[1]
  )

  # Process additional arguments to the statistic function
  extra_args <- c(extra_args, biomarker = TRUE, ...)

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_response_subgroups) <- c(formals(a_response_subgroups), extra_args[[".additional_fun_parameters"]])

  # Create "ci" column from "lcl" and "ucl"
  df$ci <- combine_vectors(df$lcl, df$ucl)

  df_subs <- split(df, f = df$biomarker)
  tbls <- lapply(
    df_subs,
    function(df) {
      lyt <- basic_table()

      # Split cols by the multiple variables to populate into columns.
      lyt <- split_cols_by_multivar(
        lyt = lyt,
        vars = colvars$vars,
        varlabels = colvars$labels
      )

      # Row split by biomarker
      lyt <- split_rows_by(
        lyt = lyt,
        var = "biomarker_label",
        nested = FALSE
      )

      # Add "All Patients" row
      lyt <- split_rows_by(
        lyt = lyt,
        var = "row_type",
        split_fun = keep_split_levels("content"),
        nested = TRUE,
        child_labels = "hidden"
      )
      lyt <- analyze_colvars(
        lyt = lyt,
        afun = a_response_subgroups,
        na_str = na_str,
        extra_args = c(extra_args, overall = TRUE)
      )

      # Add analysis rows
      if ("analysis" %in% df$row_type) {
        lyt <- split_rows_by(
          lyt = lyt,
          var = "row_type",
          split_fun = keep_split_levels("analysis"),
          nested = TRUE,
          child_labels = "hidden"
        )
        lyt <- split_rows_by(
          lyt = lyt,
          var = "var_label",
          nested = TRUE,
          indent_mod = 1L
        )
        lyt <- analyze_colvars(
          lyt = lyt,
          afun = a_response_subgroups,
          na_str = na_str,
          inclNAs = TRUE,
          extra_args = extra_args
        )
      }
      build_table(lyt, df = df)
    }
  )

  result <- do.call(rbind, tbls)

  n_id <- grep("n_tot", vars)
  or_id <- match("or", vars)
  ci_id <- match("ci", vars)
  structure(
    result,
    forest_header = paste0(c("Lower", "Higher"), "\nBetter"),
    col_x = or_id,
    col_ci = ci_id,
    col_symbol_size = n_id
  )
}

#' Prepare response data estimates for multiple biomarkers in a single data frame
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Prepares estimates for number of responses, patients and overall response rate,
#' as well as odds ratio estimates, confidence intervals and p-values,
#' for multiple biomarkers across population subgroups in a single data frame.
#' `variables` corresponds to the names of variables found in `data`, passed as a
#' named list and requires elements `rsp` and `biomarkers` (vector of continuous
#' biomarker variables) and optionally `covariates`, `subgroups` and `strata`.
#' `groups_lists` optionally specifies groupings for `subgroups` variables.
#'
#' @inheritParams argument_convention
#' @inheritParams response_subgroups
#' @param control (named `list`)\cr controls for the response definition and the
#'   confidence level produced by [control_logistic()].
#'
#' @return A `data.frame` with columns `biomarker`, `biomarker_label`, `n_tot`, `n_rsp`,
#'   `prop`, `or`, `lcl`, `ucl`, `conf_level`, `pval`, `pval_label`, `subgroup`, `var`,
#'   `var_label`, and `row_type`.
#'
#' @note You can also specify a continuous variable in `rsp` and then use the
#'   `response_definition` control to convert that internally to a logical
#'   variable reflecting binary response.
#'
#' @seealso [h_logistic_mult_cont_df()] which is used internally.
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
#'   mutate(rsp = AVALC == "CR")
#'
#' # Typical analysis of two continuous biomarkers `BMRKR1` and `AGE`,
#' # in logistic regression models with one covariate `RACE`. The subgroups
#' # are defined by the levels of `BMRKR2`.
#' df <- extract_rsp_biomarkers(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   data = adrs_f
#' )
#' df
#'
#' # Here we group the levels of `BMRKR2` manually, and we add a stratification
#' # variable `STRATA1`. We also here use a continuous variable `EOSDY`
#' # which is then binarized internally (response is defined as this variable
#' # being larger than 750).
#' df_grouped <- extract_rsp_biomarkers(
#'   variables = list(
#'     rsp = "EOSDY",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     subgroups = "BMRKR2",
#'     strata = "STRATA1"
#'   ),
#'   data = adrs_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   ),
#'   control = control_logistic(
#'     response_definition = "I(response > 750)"
#'   )
#' )
#' df_grouped
#'
#' @export
extract_rsp_biomarkers <- function(variables,
                                   data,
                                   groups_lists = list(),
                                   control = control_logistic(),
                                   label_all = "All Patients") {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `extract_rsp_biomarkers() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

  assert_list_of_variables(variables)
  checkmate::assert_string(variables$rsp)
  checkmate::assert_character(variables$subgroups, null.ok = TRUE)
  checkmate::assert_string(label_all)

  # Start with all patients.
  result_all <- h_logistic_mult_cont_df(
    variables = variables,
    data = data,
    control = control
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"
  if (is.null(variables$subgroups)) {
    # Only return result for all patients.
    result_all
  } else {
    # Add subgroups results.
    l_data <- h_split_by_subgroups(
      data,
      variables$subgroups,
      groups_lists = groups_lists
    )
    l_result <- lapply(l_data, function(grp) {
      result <- h_logistic_mult_cont_df(
        variables = variables,
        data = grp$df,
        control = control
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
