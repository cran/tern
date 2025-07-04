#' Tabulate biomarker effects on survival by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [tabulate_survival_biomarkers()] function creates a layout element to tabulate the estimated effects of multiple
#' continuous biomarker variables on survival across subgroups, returning statistics including median survival time and
#' hazard ratio for each population subgroup. The table is created from `df`, a list of data frames returned by
#' [extract_survival_biomarkers()], with the statistics to include specified via the `vars` parameter.
#'
#' A forest plot can be created from the resulting table using the [g_forest()] function.
#'
#' @inheritParams fit_coxreg_multivar
#' @inheritParams survival_duration_subgroups
#' @inheritParams argument_convention
#' @param df (`data.frame`)\cr containing all analysis variables, as returned by
#'   [extract_survival_biomarkers()].
#' @param vars (`character`)\cr the names of statistics to be reported among:
#'   * `n_tot_events`: Total number of events per group.
#'   * `n_tot`: Total number of observations per group.
#'   * `median`: Median survival time.
#'   * `hr`: Hazard ratio.
#'   * `ci`: Confidence interval of hazard ratio.
#'   * `pval`: p-value of the effect.
#'   Note, one of the statistics `n_tot` and `n_tot_events`, as well as both `hr` and `ci` are required.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. The tables are then typically used as input for forest plots.
#'
#' @examples
#' library(dplyr)
#'
#' adtte <- tern_ex_adtte
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- formatters::var_labels(adtte)
#'
#' adtte_f <- adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVALU = as.character(AVALU),
#'     is_event = CNSR == 0
#'   )
#' labels <- c("AVALU" = adtte_labels[["AVALU"]], "is_event" = "Event Flag")
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
#'
#' # Typical analysis of two continuous biomarkers `BMRKR1` and `AGE`,
#' # in multiple regression models containing one covariate `RACE`,
#' # as well as one stratification variable `STRATA1`. The subgroups
#' # are defined by the levels of `BMRKR2`.
#'
#' df <- extract_survival_biomarkers(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     strata = "STRATA1",
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   label_all = "Total Patients",
#'   data = adtte_f
#' )
#' df
#'
#' # Here we group the levels of `BMRKR2` manually.
#' df_grouped <- extract_survival_biomarkers(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     strata = "STRATA1",
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   data = adtte_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   )
#' )
#' df_grouped
#'
#' @name survival_biomarkers_subgroups
#' @order 1
NULL

#' Prepare survival data estimates for multiple biomarkers in a single data frame
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Prepares estimates for number of events, patients and median survival times, as well as hazard ratio estimates,
#' confidence intervals and p-values, for multiple biomarkers across population subgroups in a single data frame.
#' `variables` corresponds to the names of variables found in `data`, passed as a named `list` and requires elements
#' `tte`, `is_event`, `biomarkers` (vector of continuous biomarker variables), and optionally `subgroups` and `strata`.
#' `groups_lists` optionally specifies groupings for `subgroups` variables.
#'
#' @inheritParams argument_convention
#' @inheritParams fit_coxreg_multivar
#' @inheritParams survival_duration_subgroups
#'
#' @return A `data.frame` with columns `biomarker`, `biomarker_label`, `n_tot`, `n_tot_events`,
#'   `median`, `hr`, `lcl`, `ucl`, `conf_level`, `pval`, `pval_label`, `subgroup`, `var`,
#'   `var_label`, and `row_type`.
#'
#' @seealso [h_coxreg_mult_cont_df()] which is used internally, [tabulate_survival_biomarkers()].
#'
#' @export
extract_survival_biomarkers <- function(variables,
                                        data,
                                        groups_lists = list(),
                                        control = control_coxreg(),
                                        label_all = "All Patients") {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `extract_survival_biomarkers() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

  checkmate::assert_list(variables)
  checkmate::assert_character(variables$subgroups, null.ok = TRUE)
  checkmate::assert_string(label_all)

  # Start with all patients.
  result_all <- h_coxreg_mult_cont_df(
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
      result <- h_coxreg_mult_cont_df(
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

#' @describeIn survival_biomarkers_subgroups Table-creating function which creates a table
#'   summarizing biomarker effects on survival by subgroup.
#'
#' @param label_all `r lifecycle::badge("deprecated")`\cr please assign the `label_all` parameter within the
#'   [extract_survival_biomarkers()] function when creating `df`.
#'
#' @return An `rtables` table summarizing biomarker effects on survival by subgroup.
#'
#' @note In contrast to [tabulate_survival_subgroups()] this tabulation function does
#'   not start from an input layout `lyt`. This is because internally the table is
#'   created by combining multiple subtables.
#'
#' @seealso [extract_survival_biomarkers()]
#'
#' @examples
#' ## Table with default columns.
#' tabulate_survival_biomarkers(df)
#'
#' ## Table with a manually chosen set of columns: leave out "pval", reorder.
#' tab <- tabulate_survival_biomarkers(
#'   df = df,
#'   vars = c("n_tot_events", "ci", "n_tot", "median", "hr"),
#'   time_unit = as.character(adtte_f$AVALU[1])
#' )
#'
#' ## Finally produce the forest plot.
#' \donttest{
#' g_forest(tab, xlim = c(0.8, 1.2))
#' }
#'
#' @export
#' @order 2
tabulate_survival_biomarkers <- function(df,
                                         vars = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
                                         groups_lists = list(),
                                         control = control_coxreg(),
                                         label_all = lifecycle::deprecated(),
                                         time_unit = NULL,
                                         na_str = default_na_str(),
                                         ...,
                                         .stat_names = NULL,
                                         .formats = NULL,
                                         .labels = NULL,
                                         .indent_mods = NULL) {
  if (lifecycle::is_present(label_all)) {
    lifecycle::deprecate_warn(
      "0.9.5", "tabulate_survival_biomarkers(label_all)",
      details = paste(
        "Please assign the `label_all` parameter within the",
        "`extract_survival_biomarkers()` function when creating `df`."
      )
    )
  }

  checkmate::assert_data_frame(df)
  checkmate::assert_character(df$biomarker)
  checkmate::assert_character(df$biomarker_label)
  checkmate::assert_subset(vars, get_stats("tabulate_survival_biomarkers"))

  # Process standard extra arguments
  extra_args <- list(".stats" = vars)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  colvars <- d_survival_subgroups_colvars(
    vars,
    conf_level = df$conf_level[1],
    method = df$pval_label[1],
    time_unit = time_unit
  )

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    groups_lists = list(groups_lists), control = list(control), biomarker = TRUE,
    ...
  )

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_survival_subgroups) <- c(formals(a_survival_subgroups), extra_args[[".additional_fun_parameters"]])

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
        afun = a_survival_subgroups,
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
          afun = a_survival_subgroups,
          na_str = na_str,
          inclNAs = TRUE,
          extra_args = extra_args
        )
      }
      build_table(lyt, df = df)
    }
  )

  result <- do.call(rbind, tbls)

  n_tot_ids <- grep("^n_tot", vars)
  hr_id <- match("hr", vars)
  ci_id <- match("ci", vars)
  structure(
    result,
    forest_header = paste0(c("Higher", "Lower"), "\nBetter"),
    col_x = hr_id,
    col_ci = ci_id,
    col_symbol_size = n_tot_ids[1]
  )
}
