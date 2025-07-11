#' Count patients by most extreme post-baseline toxicity grade per direction of abnormality
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_abnormal_by_worst_grade()] creates a layout element to count patients by highest (worst)
#' analysis toxicity grade post-baseline for each direction, categorized by parameter value.
#'
#' This function analyzes primary analysis variable `var` which indicates toxicity grades. Additional
#' analysis variables that can be supplied as a list via the `variables` parameter are `id` (defaults to
#' `USUBJID`), a variable to indicate unique subject identifiers, `param` (defaults to `PARAM`), a variable
#' to indicate parameter values, and `grade_dir` (defaults to `GRADE_DIR`), a variable to indicate directions
#' (e.g. High or Low) for each toxicity grade supplied in `var`.
#'
#' For each combination of `param` and `grade_dir` levels, patient counts by worst
#' grade are calculated as follows:
#'   * `1` to `4`: The number of patients with worst grades 1-4, respectively.
#'   * `Any`: The number of patients with at least one abnormality (i.e. grade is not 0).
#'
#' Fractions are calculated by dividing the above counts by the number of patients with at least one
#' valid measurement recorded during treatment.
#'
#' Pre-processing is crucial when using this function and can be done automatically using the
#' [h_adlb_abnormal_by_worst_grade()] helper function. See the description of this function for details on the
#' necessary pre-processing steps.
#'
#' Prior to using this function in your table layout you must use [rtables::split_rows_by()] to create two row
#' splits, one on variable `param` and one on variable `grade_dir`.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("abnormal_by_worst_grade"), type = "sh")``
#'
#' @seealso [h_adlb_abnormal_by_worst_grade()] which pre-processes ADLB data frames to be used in
#'   [count_abnormal_by_worst_grade()].
#'
#' @name abnormal_by_worst_grade
#' @order 1
NULL

#' @describeIn abnormal_by_worst_grade Statistics function which counts patients by worst grade.
#'
#' @return
#' * `s_count_abnormal_by_worst_grade()` returns the single statistic `count_fraction` with grades 1 to 4 and
#'   "Any" results.
#'
#' @keywords internal
s_count_abnormal_by_worst_grade <- function(df,
                                            .var = "GRADE_ANL",
                                            .spl_context,
                                            variables = list(
                                              id = "USUBJID",
                                              param = "PARAM",
                                              grade_dir = "GRADE_DIR"
                                            ),
                                            ...) {
  checkmate::assert_string(.var)
  assert_valid_factor(df[[.var]])
  assert_valid_factor(df[[variables$param]])
  assert_valid_factor(df[[variables$grade_dir]])
  assert_df_with_variables(df, c(a = .var, variables))
  checkmate::assert_multi_class(df[[variables$id]], classes = c("factor", "character"))

  # To verify that the `split_rows_by` are performed with correct variables.
  checkmate::assert_subset(c(variables[["param"]], variables[["grade_dir"]]), .spl_context$split)
  first_row <- .spl_context[.spl_context$split == variables[["param"]], ]
  x_lvls <- c(setdiff(levels(df[[.var]]), "0"), "Any")
  result <- split(numeric(0), factor(x_lvls))

  subj <- first_row$full_parent_df[[1]][[variables[["id"]]]]
  subj_cur_col <- subj[first_row$cur_col_subset[[1]]]
  # Some subjects may have a record for high and low directions but
  # should be counted only once.
  denom <- length(unique(subj_cur_col))

  for (lvl in x_lvls) {
    if (lvl != "Any") {
      df_lvl <- df[df[[.var]] == lvl, ]
    } else {
      df_lvl <- df[df[[.var]] != 0, ]
    }
    num <- length(unique(df_lvl[[variables[["id"]]]]))
    fraction <- ifelse(denom == 0, 0, num / denom)
    result[[lvl]] <- formatters::with_label(c(count = num, fraction = fraction), lvl)
  }

  result <- list(count_fraction = result)
  result
}

#' @describeIn abnormal_by_worst_grade Formatted analysis function which is used as `afun`
#'   in `count_abnormal_by_worst_grade()`.
#'
#' @return
#' * `a_count_abnormal_by_worst_grade()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_abnormal_by_worst_grade <- function(df,
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
    default_stat_fnc = s_count_abnormal_by_worst_grade,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("abnormal_by_worst_grade", stats_in = .stats, custom_stats_in = names(custom_stat_functions))
  levels_per_stats <- lapply(x_stats, names)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(.stats, .labels, levels_per_stats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  x_stats <- x_stats[.stats] %>%
    .unlist_keep_nulls() %>%
    setNames(names(.formats))

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

#' @describeIn abnormal_by_worst_grade Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_abnormal_by_worst_grade()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_abnormal_by_worst_grade()` to the table layout.
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#' adlb <- tern_ex_adlb
#'
#' # Data is modified in order to have some parameters with grades only in one direction
#' # and simulate the real data.
#' adlb$ATOXGR[adlb$PARAMCD == "ALT" & adlb$ATOXGR %in% c("1", "2", "3", "4")] <- "-1"
#' adlb$ANRIND[adlb$PARAMCD == "ALT" & adlb$ANRIND == "HIGH"] <- "LOW"
#' adlb$WGRHIFL[adlb$PARAMCD == "ALT"] <- ""
#'
#' adlb$ATOXGR[adlb$PARAMCD == "IGA" & adlb$ATOXGR %in% c("-1", "-2", "-3", "-4")] <- "1"
#' adlb$ANRIND[adlb$PARAMCD == "IGA" & adlb$ANRIND == "LOW"] <- "HIGH"
#' adlb$WGRLOFL[adlb$PARAMCD == "IGA"] <- ""
#'
#' # Pre-processing
#' adlb_f <- adlb %>% h_adlb_abnormal_by_worst_grade()
#'
#' # Map excludes records without abnormal grade since they should not be displayed
#' # in the table.
#' map <- unique(adlb_f[adlb_f$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]) %>%
#'   lapply(as.character) %>%
#'   as.data.frame() %>%
#'   arrange(PARAM, desc(GRADE_DIR), GRADE_ANL)
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAM") %>%
#'   split_rows_by("GRADE_DIR", split_fun = trim_levels_to_map(map)) %>%
#'   count_abnormal_by_worst_grade(
#'     var = "GRADE_ANL",
#'     variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
#'   ) %>%
#'   build_table(df = adlb_f)
#'
#' @export
#' @order 2
count_abnormal_by_worst_grade <- function(lyt,
                                          var,
                                          variables = list(
                                            id = "USUBJID",
                                            param = "PARAM",
                                            grade_dir = "GRADE_DIR"
                                          ),
                                          na_str = default_na_str(),
                                          nested = TRUE,
                                          ...,
                                          .stats = "count_fraction",
                                          .stat_names = NULL,
                                          .formats = list(count_fraction = format_count_fraction),
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(extra_args, "variables" = list(variables), ...)

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_abnormal_by_worst_grade) <- c(
    formals(a_count_abnormal_by_worst_grade), extra_args[[".additional_fun_parameters"]]
  )

  analyze(
    lyt = lyt,
    vars = var,
    afun = a_count_abnormal_by_worst_grade,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = "hidden"
  )
}

#' Helper function to prepare ADLB for `count_abnormal_by_worst_grade()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to prepare an ADLB data frame to be used as input in
#' [count_abnormal_by_worst_grade()]. The following pre-processing steps are applied:
#'
#' 1. `adlb` is filtered on variable `avisit` to only include post-baseline visits.
#' 2. `adlb` is filtered on variables `worst_flag_low` and `worst_flag_high` so that only
#'    worst grades (in either direction) are included.
#' 3. From the standard lab grade variable `atoxgr`, the following two variables are derived
#'    and added to `adlb`:
#'   * A grade direction variable (e.g. `GRADE_DIR`). The variable takes value `"HIGH"` when
#'     `atoxgr > 0`, `"LOW"` when `atoxgr < 0`, and `"ZERO"` otherwise.
#'   * A toxicity grade variable (e.g. `GRADE_ANL`) where all negative values from `atoxgr` are
#'     replaced by their absolute values.
#' 4. Unused factor levels are dropped from `adlb` via [droplevels()].
#'
#' @param adlb (`data.frame`)\cr ADLB data frame.
#' @param atoxgr (`string`)\cr name of the analysis toxicity grade variable. This must be a `factor`
#'   variable.
#' @param avisit (`string`)\cr name of the analysis visit variable.
#' @param worst_flag_low (`string`)\cr name of the worst low lab grade flag variable. This variable is
#'   set to `"Y"` when indicating records of worst low lab grades.
#' @param worst_flag_high (`string`)\cr name of the worst high lab grade flag variable. This variable is
#'   set to `"Y"` when indicating records of worst high lab grades.
#'
#' @return `h_adlb_abnormal_by_worst_grade()` returns the `adlb` data frame with two new
#'   variables: `GRADE_DIR` and `GRADE_ANL`.
#'
#' @seealso [abnormal_by_worst_grade]
#'
#' @examples
#' h_adlb_abnormal_by_worst_grade(tern_ex_adlb) %>%
#'   dplyr::select(ATOXGR, GRADE_DIR, GRADE_ANL) %>%
#'   head(10)
#'
#' @export
h_adlb_abnormal_by_worst_grade <- function(adlb,
                                           atoxgr = "ATOXGR",
                                           avisit = "AVISIT",
                                           worst_flag_low = "WGRLOFL",
                                           worst_flag_high = "WGRHIFL") {
  adlb %>%
    dplyr::filter(
      !.data[[avisit]] %in% c("SCREENING", "BASELINE"),
      .data[[worst_flag_low]] == "Y" | .data[[worst_flag_high]] == "Y"
    ) %>%
    dplyr::mutate(
      GRADE_DIR = factor(
        dplyr::case_when(
          .data[[atoxgr]] %in% c("-1", "-2", "-3", "-4") ~ "LOW",
          .data[[atoxgr]] == "0" ~ "ZERO",
          .data[[atoxgr]] %in% c("1", "2", "3", "4") ~ "HIGH"
        ),
        levels = c("LOW", "ZERO", "HIGH")
      ),
      GRADE_ANL = forcats::fct_relevel(
        forcats::fct_recode(.data[[atoxgr]], `1` = "-1", `2` = "-2", `3` = "-3", `4` = "-4"),
        c("0", "1", "2", "3", "4")
      )
    ) %>%
    droplevels()
}
