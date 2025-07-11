#' Count specific values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_values()] creates a layout element to calculate counts of specific values within a
#' variable of interest.
#'
#' This function analyzes one or more variables of interest supplied as a vector to `vars`. Values to
#' count for variable(s) in `vars` can be given as a vector via the `values` argument. One row of
#' counts will be generated for each variable.
#'
#' @inheritParams argument_convention
#' @param values (`character`)\cr specific values that should be counted.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_values"), type = "sh")``
#'
#' @note
#' * For `factor` variables, `s_count_values` checks whether `values` are all included in the levels of `x`
#'   and fails otherwise.
#' * For `count_values()`, variable labels are shown when there is more than one element in `vars`,
#'   otherwise they are hidden.
#'
#' @name count_values
#' @order 1
NULL

#' @describeIn count_values S3 generic function to count values.
#'
#' @inheritParams s_summary.logical
#'
#' @return
#' * `s_count_values()` returns output of [s_summary()] for specified values of a non-numeric variable.
#'
#' @export
s_count_values <- function(x,
                           values,
                           na.rm = TRUE, # nolint
                           denom = c("n", "N_col", "N_row"),
                           ...) {
  UseMethod("s_count_values", x)
}

#' @describeIn count_values Method for `character` class.
#'
#' @method s_count_values character
#'
#' @examples
#' # `s_count_values.character`
#' s_count_values(x = c("a", "b", "a"), values = "a")
#' s_count_values(x = c("a", "b", "a", NA, NA), values = "b", na.rm = FALSE)
#'
#' @export
s_count_values.character <- function(x,
                                     values = "Y",
                                     na.rm = TRUE, # nolint
                                     ...) {
  checkmate::assert_character(values)

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  is_in_values <- x %in% values

  s_summary(is_in_values, na_rm = na.rm, ...)
}

#' @describeIn count_values Method for `factor` class. This makes an automatic
#'   conversion to `character` and then forwards to the method for characters.
#'
#' @method s_count_values factor
#'
#' @examples
#' # `s_count_values.factor`
#' s_count_values(x = factor(c("a", "b", "a")), values = "a")
#'
#' @export
s_count_values.factor <- function(x,
                                  values = "Y",
                                  ...) {
  s_count_values(as.character(x), values = as.character(values), ...)
}

#' @describeIn count_values Method for `logical` class.
#'
#' @method s_count_values logical
#'
#' @examples
#' # `s_count_values.logical`
#' s_count_values(x = c(TRUE, FALSE, TRUE))
#'
#' @export
s_count_values.logical <- function(x, values = TRUE, ...) {
  checkmate::assert_logical(values)
  s_count_values(as.character(x), values = as.character(values), ...)
}

#' @describeIn count_values Formatted analysis function which is used as `afun`
#'   in `count_values()`.
#'
#' @return
#' * `a_count_values()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' # `a_count_values`
#' a_count_values(x = factor(c("a", "b", "a")), values = "a", .N_col = 10, .N_row = 10)
#'
#' @export
a_count_values <- function(x,
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

  # Main statistic calculations
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_values,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      x = list(x),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("analyze_vars_counts", stats_in = .stats, custom_stats_in = names(custom_stat_functions))
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  x_stats <- x_stats[.stats]

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn count_values Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_values()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_values()` to the table layout.
#'
#' @examples
#' # `count_values`
#' basic_table() %>%
#'   count_values("Species", values = "setosa") %>%
#'   build_table(iris)
#'
#' @export
#' @order 2
count_values <- function(lyt,
                         vars,
                         values,
                         na_str = default_na_str(),
                         na_rm = TRUE,
                         nested = TRUE,
                         ...,
                         table_names = vars,
                         .stats = "count_fraction",
                         .stat_names = NULL,
                         .formats = c(count_fraction = "xx (xx.xx%)", count = "xx"),
                         .labels = c(count_fraction = paste(values, collapse = ", ")),
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
    na_rm = na_rm, values = list(values),
    ...
  )

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_values) <- c(formals(a_count_values), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt,
    vars,
    afun = a_count_values,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
    table_names = table_names
  )
}
