#' Standard arguments
#'
#' The documentation to this function lists all the arguments in `tern`
#' that are used repeatedly to express an analysis.
#'
#' @param ... additional arguments for the lower level functions.
#' @param .aligns (`character` or `NULL`)\cr alignment for table contents (not including labels). When `NULL`,
#'   `"center"` is applied. See [formatters::list_valid_aligns()] for a list of all currently supported alignments.
#' @param .all_col_counts (`integer`)\cr vector where each value represents a global count for a column. Values are
#'   taken from `alt_counts_df` if specified (see [rtables::build_table()]).
#' @param .df_row (`data.frame`)\cr data frame across all of the columns for the given row split.
#' @param .formats (named `character` or `list`)\cr formats for the statistics. See Details in `analyze_vars` for more
#'   information on the `"auto"` setting.
#' @param .in_ref_col (`flag`)\cr `TRUE` when working with the reference level, `FALSE` otherwise.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels. Defaults to 0, which corresponds to the
#'   unmodified default behavior. Can be negative.
#' @param .labels (named `character`)\cr labels for the statistics (without indent).
#' @param .N_col (`integer(1)`)\cr column-wise N (column count) for the full column being analyzed that is typically
#'   passed by `rtables`.
#' @param .N_row (`integer(1)`)\cr row-wise N (row group count) for the group of observations being analyzed
#'   (i.e. with no column-based subsetting) that is typically passed by `rtables`.
#' @param .ref_group (`data.frame` or `vector`)\cr the data corresponding to the reference group.
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states
#'   that is passed by `rtables`.
#' @param .stats (`character`)\cr statistics to select for the table.
#' @param .stat_names (`character`)\cr names of the statistics that are passed directly to name single statistics
#'   (`.stats`). This option is visible when producing [rtables::as_result_df()] with `make_ard = TRUE`.
#' @param .var (`string`)\cr single variable name that is passed by `rtables` when requested
#'   by a statistics function.
#' @param add_total_level (`flag`)\cr adds a "total" level after the others which includes all the levels
#'   that constitute the split. A custom label can be set for this level via the `custom_label` argument.
#' @param col_by (`factor`)\cr defining column groups.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param data (`data.frame`)\cr the dataset containing the variables to summarize.
#' @param denom (`string`)\cr choice of denominator for proportion. Options are:
#'   * `n`: number of values in this row and column intersection.
#'   * `N_row`: total number of values in this row across columns.
#'   * `N_col`: total number of values in this column across rows.
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param groups_lists (named `list` of `list`)\cr optionally contains for each `subgroups` variable a
#'   list, which specifies the new group levels via the names and the
#'   levels that belong to it in the character vectors that are elements of the list.
#' @param id (`string`)\cr subject variable name.
#' @param is_event (`flag`)\cr `TRUE` if event, `FALSE` if time to event is censored.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @param labelstr (`string`)\cr label of the level of the parent split currently being summarized
#'   (must be present as second argument in Content Row Functions). See [rtables::summarize_row_groups()]
#'   for more information.
#' @param lyt (`PreDataTableLayouts`)\cr layout that analyses will be added to.
#' @param method (`string` or `NULL`)\cr specifies the test used to calculate the p-value for the difference between
#'   two proportions. For options, see [test_proportion_diff()]. Default is `NULL` so no test is performed.
#' @param na.rm (`flag`)\cr whether `NA` values should be removed from `x` prior to analysis.
#' @param na_rm (`flag`)\cr whether `NA` values should be removed from `x` prior to analysis.
#' @param na_str (`string`)\cr string used to replace all `NA` or empty values in the output.
#' @param nested (`flag`)\cr whether this layout instruction should be applied within the existing layout structure _if
#'   possible (`TRUE`, the default) or as a new top-level element (`FALSE`). Ignored if it would nest a split.
#'   underneath analyses, which is not allowed.
#' @param prune_zero_rows (`flag`)\cr whether to prune all zero rows.
#' @param riskdiff (`flag`)\cr whether a risk difference column is present. When set to `TRUE`, [add_riskdiff()] must be
#'   used as `split_fun` in the prior column split of the table layout, specifying which columns should be compared.
#'   See [stat_propdiff_ci()] for details on risk difference calculation.
#' @param rsp (`logical`)\cr vector indicating whether each subject is a responder or not.
#' @param show_labels (`string`)\cr label visibility: one of "default", "visible" and "hidden".
#' @param section_div (`string`)\cr string which should be repeated as a section divider after each group
#'   defined by this split instruction, or `NA_character_` (the default) for no section divider.
#' @param table_names (`character`)\cr this can be customized in the case that the same `vars` are analyzed multiple
#'   times, to avoid warnings from `rtables`.
#' @param tte (`numeric`)\cr vector of time-to-event duration values.
#' @param var_labels (`character`)\cr variable labels.
#' @param variables (named `list` of `string`)\cr list of additional analysis variables.
#' @param vars (`character`)\cr variable names for the primary analysis variable to be iterated over.
#' @param var (`string`)\cr single variable name for the primary analysis variable.
#' @param x (`numeric`)\cr vector of numbers we want to analyze.
#' @param xlim (`numeric(2)`)\cr vector containing lower and upper limits for the x-axis, respectively.
#'   If `NULL` (default), the default scale range is used.
#' @param ylim (`numeric(2)`)\cr vector containing lower and upper limits for the y-axis, respectively.
#'   If `NULL` (default), the default scale range is used.
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
