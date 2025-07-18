#' Estimate proportions of each level of a variable
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze & summarize function [estimate_multinomial_response()] creates a layout element to estimate the
#' proportion and proportion confidence interval for each level of a factor variable. The primary analysis variable,
#' `var`, should be a factor variable, the values of which will be used as labels within the output table.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("estimate_multinomial_response"), type = "sh")``
#'
#' @seealso Relevant description function [d_onco_rsp_label()].
#'
#' @name estimate_multinomial_rsp
#' @order 1
NULL

#' Description of standard oncology response
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Describe the oncology response in a standard way.
#'
#' @param x (`character`)\cr the standard oncology codes to be described.
#'
#' @return Response labels.
#'
#' @seealso [estimate_multinomial_rsp()]
#'
#' @examples
#' d_onco_rsp_label(
#'   c("CR", "PR", "SD", "NON CR/PD", "PD", "NE", "Missing", "<Missing>", "NE/Missing")
#' )
#'
#' # Adding some values not considered in d_onco_rsp_label
#'
#' d_onco_rsp_label(
#'   c("CR", "PR", "hello", "hi")
#' )
#'
#' @export
d_onco_rsp_label <- function(x) {
  x <- as.character(x)
  desc <- c(
    CR           = "Complete Response (CR)",
    PR           = "Partial Response (PR)",
    MR           = "Minimal/Minor Response (MR)",
    MRD          = "Minimal Residual Disease (MRD)",
    SD           = "Stable Disease (SD)",
    PD           = "Progressive Disease (PD)",
    `NON CR/PD`  = "Non-CR or Non-PD (NON CR/PD)",
    NE           = "Not Evaluable (NE)",
    `NE/Missing` = "Missing or unevaluable",
    Missing      = "Missing",
    `NA`         = "Not Applicable (NA)",
    ND           = "Not Done (ND)"
  )

  values_label <- vapply(
    X = x,
    FUN.VALUE = character(1),
    function(val) {
      if (val %in% names(desc)) desc[val] else val
    }
  )

  factor(values_label, levels = c(intersect(desc, values_label), setdiff(values_label, desc)))
}

#' @describeIn estimate_multinomial_rsp Statistics function which feeds the length of `x` as number
#'   of successes, and `.N_col` as total number of successes and failures into [s_proportion()].
#'
#' @return
#' * `s_length_proportion()` returns statistics from [s_proportion()].
#'
#' @examples
#' s_length_proportion(rep("CR", 10), .N_col = 100)
#' s_length_proportion(factor(character(0)), .N_col = 100)
#'
#' @export
s_length_proportion <- function(x,
                                ...,
                                .N_col) { # nolint
  checkmate::assert_multi_class(x, classes = c("factor", "character"))
  checkmate::assert_vector(x, min.len = 0, max.len = .N_col)
  checkmate::assert_vector(unique(x), min.len = 0, max.len = 1)

  n_true <- length(x)
  n_false <- .N_col - n_true
  x_logical <- rep(c(TRUE, FALSE), c(n_true, n_false))
  s_proportion(df = x_logical, ...)
}

#' @describeIn estimate_multinomial_rsp Formatted analysis function which is used as `afun`
#'   in `estimate_multinomial_response()`.
#'
#' @return
#' * `a_length_proportion()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_length_proportion(rep("CR", 10), .N_col = 100)
#' a_length_proportion(factor(character(0)), .N_col = 100)
#'
#' @export
a_length_proportion <- function(x,
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
    default_stat_fnc = s_length_proportion,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      x = list(x),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("estimate_multinomial_response",
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

#' @describeIn estimate_multinomial_rsp Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()] and
#'   [rtables::summarize_row_groups()].
#'
#' @return
#' * `estimate_multinomial_response()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_length_proportion()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' # Use of the layout creating function.
#' dta_test <- data.frame(
#'   USUBJID = paste0("S", 1:12),
#'   ARM     = factor(rep(LETTERS[1:3], each = 4)),
#'   AVAL    = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
#' ) %>% mutate(
#'   AVALC = factor(AVAL,
#'     levels = c(0, 1),
#'     labels = c("Complete Response (CR)", "Partial Response (PR)")
#'   )
#' )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   estimate_multinomial_response(var = "AVALC")
#'
#' tbl <- build_table(lyt, dta_test)
#'
#' tbl
#'
#' @export
#' @order 2
estimate_multinomial_response <- function(lyt,
                                          var,
                                          na_str = default_na_str(),
                                          nested = TRUE,
                                          ...,
                                          show_labels = "hidden",
                                          table_names = var,
                                          .stats = "prop_ci",
                                          .stat_names = NULL,
                                          .formats = list(prop_ci = "(xx.xx, xx.xx)"),
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(extra_args, ...)

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_length_proportion) <- c(formals(a_length_proportion), extra_args[[".additional_fun_parameters"]])

  lyt <- split_rows_by(lyt, var = var)
  lyt <- summarize_row_groups(lyt, na_str = na_str)

  analyze(
    lyt = lyt,
    vars = var,
    afun = a_length_proportion,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names
  )
}
