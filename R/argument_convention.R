#' Standard Arguments
#'
#' The documentation to this function lists all the arguments in `tern`
#' that are used repeatedly to express an analysis.
#'
#' @param ... additional arguments for the lower level functions.
#' @param .df_row (`data.frame`)\cr data frame across all of the columns for the given row split.
#' @param .in_ref_col (`logical`)\cr `TRUE` when working with the reference level, `FALSE` otherwise.
#' @param .N_col (`count`)\cr row-wise N (row group count) for the group of observations being analyzed
#'   (i.e. with no column-based subsetting) that is passed by `rtables`.
#' @param .N_row (`count`)\cr column-wise N (column count) for the full column that is passed by `rtables`.
#' @param .ref_group (`data.frame` or `vector`)\cr the data corresponding to the reference group.
#' @param .stats (`character`)\cr statistics to select for the table.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels. Defaults to 0, which corresponds to the
#'   unmodified default behavior. Can be negative.
#' @param .formats (named `character` or `list`)\cr formats for the statistics.
#' @param .labels (named `character`)\cr labels for the statistics (without indent).
#' @param .var (`string`)\cr single variable name that is passed by `rtables` when requested
#'   by a statistics function.
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states
#'   that is passed by `rtables`.
#' @param add_total_level (`flag`)\cr adds a "total" level after the others which includes all the levels
#'   that constitute the split. A custom label can be set for this level via the `custom_label` argument.
#' @param col_by (`factor`)\cr defining column groups.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param data (`data.frame`)\cr the dataset containing the variables to summarize.
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param draw (`flag`)\cr whether the plot should be drawn.
#' @param drop (`flag`)\cr should non appearing occurrence levels be dropped from the resulting table.
#'   Note that in that case the remaining occurrence levels in the table are sorted alphabetically.
#' @param id (`string`)\cr subject variable name.
#' @param is_event (`logical`)\cr `TRUE` if event, `FALSE` if time to event is censored.
#' @param indent_mod `r lifecycle::badge("deprecated")` Please use the `.indent_mods` argument instead.
#' @param labelstr (`character`)\cr label of the level of the parent split currently being summarized
#'   (must be present as second argument in Content Row Functions). See [rtables::summarize_row_groups()]
#'   for more information.
#' @param lyt (`layout`)\cr input layout where analyses will be added to.
#' @param na.rm (`flag`)\cr whether `NA` values should be removed from `x` prior to analysis.
#' @param na_level (`string`)\cr string used to replace all `NA` or empty values in the output.
#' @param nested (`flag`)\cr whether this layout instruction be applied within the existing layout structure _if
#'   possible_ (`TRUE`, the default) or as a new top-level element (`FALSE`). Ignored if it would nest a split
#'   underneath analyses, which is not allowed.
#' @param newpage (`flag`)\cr whether the plot should be drawn on a new page.
#'   Only considered if `draw = TRUE` is used.
#' @param prune_zero_rows (`flag`)\cr whether to prune all zero rows.
#' @param rsp (`logical`)\cr whether each subject is a responder or not.
#' @param show_labels (`string`)\cr label visibility: one of "default", "visible" and "hidden".
#' @param section_div (`string`)\cr string which should be repeated as a section divider after each group
#'   defined by this split instruction, or `NA_character_` (the default) for no section divider.
#' @param table_names (`character`)\cr this can be customized in case that the same `vars` are analyzed multiple times,
#'   to avoid warnings from `rtables`.
#' @param tte (`numeric`)\cr contains time-to-event duration values.
#' @param var_labels (`character`)\cr character for label.
#' @param variables (named `list` of `string`)\cr list of additional analysis variables.
#' @param vars (`character`)\cr variable names for the primary analysis variable to be iterated over.
#' @param var (`string`)\cr single variable name for the primary analysis variable.
#' @param x (`numeric`)\cr vector of numbers we want to analyze.
#'
#' @details Although this function just returns `NULL` it has two uses, for
#'   the `tern` users it provides a documentation of arguments that are
#'   commonly and consistently used in the framework. For the developer it adds a
#'   single reference point to import the `roxygen` argument description with:
#'   `@inheritParams argument_convention`
#'
#' @keywords internal
#' @name argument_convention
NULL