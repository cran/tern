#' Line plot with optional table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Line plot with optional table.
#'
#' @inheritParams argument_convention
#' @param alt_counts_df (`data.frame` or `NULL`)\cr data set that will be used (only)
#'   to counts objects in groups for stratification.
#' @param variables (named `character`) vector of variable names in `df` which should include:
#'   * `x` (`string`)\cr name of x-axis variable.
#'   * `y` (`string`)\cr name of y-axis variable.
#'   * `group_var` (`string` or `NULL`)\cr name of grouping variable (or strata), i.e. treatment arm.
#'     Can be `NA` to indicate lack of groups.
#'   * `subject_var` (`string` or `NULL`)\cr name of subject variable. Only applies if `group_var` is
#'      not NULL.
#'   * `paramcd` (`string` or `NA`)\cr name of the variable for parameter's code. Used for y-axis label and plot's
#'     subtitle. Can be `NA` if `paramcd` is not to be added to the y-axis label or subtitle.
#'   * `y_unit` (`string` or `NA`)\cr name of variable with units of `y`. Used for y-axis label and plot's subtitle.
#'     Can be `NA` if y unit is not to be added to the y-axis label or subtitle.
#'   * `facet_var` (`string` or `NA`)\cr name of the secondary grouping variable used for plot faceting, i.e. treatment
#'     arm. Can be `NA` to indicate lack of groups.
#' @param mid (`character` or `NULL`)\cr names of the statistics that will be plotted as midpoints.
#'   All the statistics indicated in `mid` variable must be present in the object returned by `sfun`,
#'   and be of a `double` or `numeric` type vector of length one.
#' @param interval (`character` or `NULL`)\cr names of the statistics that will be plotted as intervals.
#'   All the statistics indicated in `interval` variable must be present in the object returned by `sfun`,
#'   and be of a `double` or `numeric` type vector of length two. Set `interval = NULL` if intervals should not be
#'   added to the plot.
#' @param whiskers (`character`)\cr names of the interval whiskers that will be plotted. Names must match names
#'   of the list element `interval` that will be returned by `sfun` (e.g. `mean_ci_lwr` element of
#'   `sfun(x)[["mean_ci"]]`). It is possible to specify one whisker only, or to suppress all whiskers by setting
#'   `interval = NULL`.
#' @param table (`character` or `NULL`)\cr names of the statistics that will be displayed in the table below the plot.
#'   All the statistics indicated in `table` variable must be present in the object returned by `sfun`.
#' @param sfun (`function`)\cr the function to compute the values of required statistics. It must return a named `list`
#'   with atomic vectors. The names of the `list` elements refer to the names of the statistics and are used by `mid`,
#'   `interval`, `table`. It must be able to accept as input a vector with data for which statistics are computed.
#' @param ... optional arguments to `sfun`.
#' @param mid_type (`string`)\cr controls the type of the `mid` plot, it can be point (`"p"`), line (`"l"`),
#'   or point and line (`"pl"`).
#' @param mid_point_size (`numeric(1)`)\cr font size of the `mid` plot points.
#' @param position (`character` or `call`)\cr geom element position adjustment, either as a string, or the result of
#'   a call to a position adjustment function.
#' @param legend_title (`string`)\cr legend title.
#' @param legend_position (`string`)\cr the position of the plot legend (`"none"`, `"left"`, `"right"`, `"bottom"`,
#'   `"top"`, or a two-element numeric vector).
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to control styling of the plot.
#' @param xticks (`numeric` or `NULL`)\cr numeric vector of tick positions or a single number with spacing
#'   between ticks on the x-axis, for use when `variables$x` is numeric. If `NULL` (default), [labeling::extended()] is
#'   used to determine optimal tick positions on the x-axis. If `variables$x` is not numeric, this argument is ignored.
#' @param x_lab (`string` or `NULL`)\cr x-axis label. If `NULL` then no label will be added.
#' @param y_lab (`string` or `NULL`)\cr y-axis label. If `NULL` then no label will be added.
#' @param y_lab_add_paramcd (`flag`)\cr whether `paramcd`, i.e. `unique(df[[variables["paramcd"]]])` should be added
#'   to the y-axis label (`y_lab`).
#' @param y_lab_add_unit (`flag`)\cr whether y-axis unit, i.e. `unique(df[[variables["y_unit"]]])` should be added
#'   to the y-axis label (`y_lab`).
#' @param title (`string`)\cr plot title.
#' @param subtitle (`string`)\cr plot subtitle.
#' @param subtitle_add_paramcd (`flag`)\cr whether `paramcd`, i.e. `unique(df[[variables["paramcd"]]])` should be
#'   added to the plot's subtitle (`subtitle`).
#' @param subtitle_add_unit (`flag`)\cr whether the y-axis unit, i.e. `unique(df[[variables["y_unit"]]])` should be
#'   added to the plot's subtitle (`subtitle`).
#' @param caption (`string`)\cr optional caption below the plot.
#' @param table_format (named `vector` or `NULL`)\cr custom formats for descriptive statistics used instead of defaults
#'   in the (optional) table appended to the plot. It is passed directly to the `h_format_row` function through
#'   the `format` parameter. Names of `table_format` must match the names of statistics returned by `sfun` function.
#'   Can be a character vector with values from [formatters::list_valid_format_labels()] or custom format functions.
#' @param table_labels (named `character` or `NULL`)\cr labels for descriptive statistics used in the (optional) table
#'   appended to the plot. Names of `table_labels` must match the names of statistics returned by `sfun` function.
#' @param table_font_size (`numeric(1)`)\cr font size of the text in the table.
#' @param newpage `r lifecycle::badge("deprecated")` not used.
#' @param col (`character`)\cr color(s). See `?ggplot2::aes_colour_fill_alpha` for example values.
#' @param linetype (`character`)\cr line type(s). See `?ggplot2::aes_linetype_size_shape` for example values.
#' @param errorbar_width (`numeric(1)`)\cr width of the error bars.
#' @param rel_height_plot (`proportion`)\cr proportion of total figure height to allocate to the line plot.
#'   Relative height of annotation table is then `1 - rel_height_plot`. If `table = NULL`, this parameter is ignored.
#' @param as_list (`flag`)\cr whether the two `ggplot` objects should be returned as a list when `table` is not `NULL`.
#'   If `TRUE`, a named list with two elements, `plot` and `table`, will be returned. If `FALSE` (default) the
#'   annotation table is printed below the plot via [cowplot::plot_grid()].
#'
#' @return A `ggplot` line plot (and statistics table if applicable).
#'
#' @examples
#'
#' adsl <- tern_ex_adsl
#' adlb <- tern_ex_adlb %>% dplyr::filter(ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
#' adlb$AVISIT <- droplevels(adlb$AVISIT)
#' adlb <- dplyr::mutate(adlb, AVISIT = forcats::fct_reorder(AVISIT, AVISITN, min))
#'
#' # Mean with CI
#' g_lineplot(adlb, adsl, subtitle = "Laboratory Test:")
#'
#' # Mean with CI, no stratification with group_var
#' g_lineplot(adlb, variables = control_lineplot_vars(group_var = NA))
#'
#' # Mean, upper whisker of CI, no group_var(strata) counts N
#' g_lineplot(
#'   adlb,
#'   whiskers = "mean_ci_upr",
#'   title = "Plot of Mean and Upper 95% Confidence Limit by Visit"
#' )
#'
#' # Median with CI
#' g_lineplot(
#'   adlb,
#'   adsl,
#'   mid = "median",
#'   interval = "median_ci",
#'   whiskers = c("median_ci_lwr", "median_ci_upr"),
#'   title = "Plot of Median and 95% Confidence Limits by Visit"
#' )
#'
#' # Mean, +/- SD
#' g_lineplot(adlb, adsl,
#'   interval = "mean_sdi",
#'   whiskers = c("mean_sdi_lwr", "mean_sdi_upr"),
#'   title = "Plot of Median +/- SD by Visit"
#' )
#'
#' # Mean with CI plot with stats table
#' g_lineplot(adlb, adsl, table = c("n", "mean", "mean_ci"))
#'
#' # Mean with CI, table and customized confidence level
#' g_lineplot(
#'   adlb,
#'   adsl,
#'   table = c("n", "mean", "mean_ci"),
#'   control = control_analyze_vars(conf_level = 0.80),
#'   title = "Plot of Mean and 80% Confidence Limits by Visit"
#' )
#'
#' # Mean with CI, table with customized formats/labels
#' g_lineplot(
#'   adlb,
#'   adsl,
#'   table = c("n", "mean", "mean_ci"),
#'   table_format = list(
#'     mean = function(x, ...) {
#'       ifelse(x < 20, round_fmt(x, digits = 3), round_fmt(x, digits = 2))
#'     },
#'     mean_ci = "(xx.xxx, xx.xxx)"
#'   ),
#'   table_labels = list(
#'     mean = "mean",
#'     mean_ci = "95% CI"
#'   )
#' )
#'
#' # Mean with CI, table, filtered data
#' adlb_f <- dplyr::filter(adlb, ARMCD != "ARM A" | AVISIT == "BASELINE")
#' g_lineplot(adlb_f, table = c("n", "mean"))
#'
#' @export
g_lineplot <- function(df,
                       alt_counts_df = NULL,
                       variables = control_lineplot_vars(),
                       mid = "mean",
                       interval = "mean_ci",
                       whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                       table = NULL,
                       sfun = s_summary,
                       ...,
                       mid_type = "pl",
                       mid_point_size = 2,
                       position = ggplot2::position_dodge(width = 0.4),
                       legend_title = NULL,
                       legend_position = "bottom",
                       ggtheme = nestcolor::theme_nest(),
                       xticks = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       x_lab = obj_label(df[[variables[["x"]]]]),
                       y_lab = NULL,
                       y_lab_add_paramcd = TRUE,
                       y_lab_add_unit = TRUE,
                       title = "Plot of Mean and 95% Confidence Limits by Visit",
                       subtitle = "",
                       subtitle_add_paramcd = TRUE,
                       subtitle_add_unit = TRUE,
                       caption = NULL,
                       table_format = NULL,
                       table_labels = NULL,
                       table_font_size = 3,
                       errorbar_width = 0.45,
                       newpage = lifecycle::deprecated(),
                       col = NULL,
                       linetype = NULL,
                       rel_height_plot = 0.5,
                       as_list = FALSE) {
  checkmate::assert_character(variables, any.missing = TRUE)
  checkmate::assert_character(mid, null.ok = TRUE)
  checkmate::assert_character(interval, null.ok = TRUE)
  checkmate::assert_character(col, null.ok = TRUE)
  checkmate::assert_character(linetype, null.ok = TRUE)
  checkmate::assert_numeric(xticks, null.ok = TRUE)
  checkmate::assert_numeric(xlim, finite = TRUE, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  checkmate::assert_numeric(ylim, finite = TRUE, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  checkmate::assert_number(errorbar_width, lower = 0)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(subtitle, null.ok = TRUE)
  assert_proportion_value(rel_height_plot)
  checkmate::assert_logical(as_list)

  if (!is.null(table)) {
    table_format <- get_formats_from_stats(table, formats_in = table_format)
    table_labels <- get_labels_from_stats(table, labels_in = table_labels) %>% .unlist_keep_nulls()
  }

  extra_args <- list(...)
  if ("control" %in% names(extra_args)) {
    if (!is.null(table) && all(table_labels == .unlist_keep_nulls(get_labels_from_stats(table)))) {
      table_labels <- table_labels %>% labels_use_control(extra_args[["control"]])
    }
  }

  if (is.character(interval)) {
    checkmate::assert_vector(whiskers, min.len = 0, max.len = 2)
  }

  if (length(whiskers) == 1) {
    checkmate::assert_character(mid)
  }

  if (is.character(mid)) {
    checkmate::assert_scalar(mid_type)
    checkmate::assert_subset(mid_type, c("pl", "p", "l"))
  }

  x <- variables[["x"]]
  y <- variables[["y"]]
  paramcd <- variables["paramcd"] # NA if paramcd == NA or it is not in variables
  y_unit <- variables["y_unit"] # NA if y_unit == NA or it is not in variables
  if (is.na(variables["group_var"])) {
    group_var <- NULL # NULL if group_var == NA or it is not in variables
  } else {
    group_var <- variables[["group_var"]]
    subject_var <- variables[["subject_var"]]
  }
  if (is.na(variables["facet_var"])) {
    facet_var <- NULL # NULL if facet_var == NA or it is not in variables
  } else {
    facet_var <- variables[["facet_var"]]
  }
  checkmate::assert_flag(y_lab_add_paramcd, null.ok = TRUE)
  checkmate::assert_flag(subtitle_add_paramcd, null.ok = TRUE)
  if ((!is.null(y_lab) && y_lab_add_paramcd) || (!is.null(subtitle) && subtitle_add_paramcd)) {
    checkmate::assert_false(is.na(paramcd))
    checkmate::assert_scalar(unique(df[[paramcd]]))
  }

  checkmate::assert_flag(y_lab_add_unit, null.ok = TRUE)
  checkmate::assert_flag(subtitle_add_unit, null.ok = TRUE)
  if ((!is.null(y_lab) && y_lab_add_unit) || (!is.null(subtitle) && subtitle_add_unit)) {
    checkmate::assert_false(is.na(y_unit))
    checkmate::assert_scalar(unique(df[[y_unit]]))
  }

  if (!is.null(group_var) && !is.null(alt_counts_df)) {
    checkmate::assert_set_equal(unique(alt_counts_df[[group_var]]), unique(df[[group_var]]))
  }

  ####################################### |
  # ---- Compute required statistics ----
  ####################################### |
  # Remove unused levels for x-axis
  if (is.factor(df[[x]])) {
    df[[x]] <- droplevels(df[[x]])
  }

  if (!is.null(facet_var) && !is.null(group_var)) {
    df_grp <- tidyr::expand(df, .data[[facet_var]], .data[[group_var]], .data[[x]]) # expand based on levels of factors
  } else if (!is.null(group_var)) {
    df_grp <- tidyr::expand(df, .data[[group_var]], .data[[x]]) # expand based on levels of factors
  } else {
    df_grp <- tidyr::expand(df, NULL, .data[[x]])
  }

  df_grp <- df_grp %>%
    dplyr::full_join(y = df[, c(facet_var, group_var, x, y)], by = c(facet_var, group_var, x), multiple = "all") %>%
    dplyr::group_by_at(c(facet_var, group_var, x))

  df_stats <- df_grp %>%
    dplyr::summarise(
      data.frame(t(do.call(c, unname(sfun(.data[[y]])[c(mid, interval)])))),
      .groups = "drop"
    )

  df_stats <- df_stats[!is.na(df_stats[[mid]]), ]

  # add number of objects N in group_var (strata)
  if (!is.null(group_var) && !is.null(alt_counts_df)) {
    strata_N <- paste0(group_var, "_N") # nolint

    df_N <- stats::aggregate(eval(parse(text = subject_var)) ~ eval(parse(text = group_var)), data = alt_counts_df, FUN = function(x) length(unique(x))) # nolint
    colnames(df_N) <- c(group_var, "N") # nolint
    df_N[[strata_N]] <- paste0(df_N[[group_var]], " (N = ", df_N$N, ")") # nolint

    # keep strata factor levels
    matches <- sapply(unique(df_N[[group_var]]), function(x) {
      regex_pattern <- gsub("([][(){}^$.|*+?\\\\])", "\\\\\\1", x)
      unique(df_N[[paste0(group_var, "_N")]])[grepl(
        paste0("^", regex_pattern),
        unique(df_N[[paste0(group_var, "_N")]])
      )]
    })
    df_N[[paste0(group_var, "_N")]] <- factor(df_N[[group_var]]) # nolint
    levels(df_N[[paste0(group_var, "_N")]]) <- unlist(matches) # nolint

    # strata_N should not be in colnames(df_stats)
    checkmate::assert_disjunct(strata_N, colnames(df_stats))

    df_stats <- merge(x = df_stats, y = df_N[, c(group_var, strata_N)], by = group_var)
  } else if (!is.null(group_var)) {
    strata_N <- group_var # nolint
  } else {
    strata_N <- NULL # nolint
  }

  ############################################### |
  # ---- Prepare certain plot's properties. ----
  ############################################### |
  # legend title
  if (is.null(legend_title) && !is.null(group_var) && legend_position != "none") {
    legend_title <- attr(df[[group_var]], "label")
  }

  # y label
  if (!is.null(y_lab)) {
    if (y_lab_add_paramcd) {
      y_lab <- paste(y_lab, unique(df[[paramcd]]))
    }

    if (y_lab_add_unit) {
      y_lab <- paste0(y_lab, " (", unique(df[[y_unit]]), ")")
    }

    y_lab <- trimws(y_lab)
  }

  # subtitle
  if (!is.null(subtitle)) {
    if (subtitle_add_paramcd) {
      subtitle <- paste(subtitle, unique(df[[paramcd]]))
    }

    if (subtitle_add_unit) {
      subtitle <- paste0(subtitle, " (", unique(df[[y_unit]]), ")")
    }

    subtitle <- trimws(subtitle)
  }

  ############################### |
  # ---- Build plot object. ----
  ############################### |
  p <- ggplot2::ggplot(
    data = df_stats,
    mapping = ggplot2::aes(
      x = .data[[x]], y = .data[[mid]],
      color = if (is.null(strata_N)) NULL else .data[[strata_N]],
      shape = if (is.null(strata_N)) NULL else .data[[strata_N]],
      lty = if (is.null(strata_N)) NULL else .data[[strata_N]],
      group = if (is.null(strata_N)) NULL else .data[[strata_N]]
    )
  )

  if (!is.null(group_var) && nlevels(df_stats[[strata_N]]) > 6) {
    p <- p +
      scale_shape_manual(values = seq(15, 15 + nlevels(df_stats[[strata_N]])))
  }

  if (!is.null(mid)) {
    # points
    if (grepl("p", mid_type, fixed = TRUE)) {
      p <- p + ggplot2::geom_point(position = position, size = mid_point_size, na.rm = TRUE)
    }

    # lines - plotted only if there is a strata grouping (group_var)
    if (grepl("l", mid_type, fixed = TRUE) && !is.null(strata_N)) {
      p <- p + ggplot2::geom_line(position = position, na.rm = TRUE)
    }
  }

  # interval
  if (!is.null(interval)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[whiskers[1]]], ymax = .data[[whiskers[max(1, length(whiskers))]]]),
        width = errorbar_width,
        position = position
      )

    if (length(whiskers) == 1) { # lwr or upr only; mid is then required
      # workaround as geom_errorbar does not provide single-direction whiskers
      p <- p +
        ggplot2::geom_linerange(
          data = df_stats[!is.na(df_stats[[whiskers]]), ], # as na.rm =TRUE does not suppress warnings
          ggplot2::aes(ymin = .data[[mid]], ymax = .data[[whiskers]]),
          position = position,
          na.rm = TRUE,
          show.legend = FALSE
        )
    }
  }

  if (is.numeric(df_stats[[x]])) {
    if (length(xticks) == 1) xticks <- seq(from = min(df_stats[[x]]), to = max(df_stats[[x]]), by = xticks)
    p <- p + ggplot2::scale_x_continuous(breaks = if (!is.null(xticks)) xticks else waiver(), limits = xlim)
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = ylim) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = legend_title,
      lty = legend_title,
      shape = legend_title,
      x = x_lab,
      y = y_lab
    )

  if (!is.null(col)) {
    p <- p +
      ggplot2::scale_color_manual(values = col)
  }
  if (!is.null(linetype)) {
    p <- p +
      ggplot2::scale_linetype_manual(values = linetype)
  }

  if (!is.null(facet_var)) {
    p <- p +
      facet_grid(cols = vars(df_stats[[facet_var]]))
  }

  if (!is.null(ggtheme)) {
    p <- p + ggtheme
  } else {
    p <- p +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.key.width = grid::unit(1, "cm"),
        legend.position = legend_position,
        legend.direction = ifelse(
          legend_position %in% c("top", "bottom"),
          "horizontal",
          "vertical"
        )
      )
  }

  ############################################################# |
  # ---- Optionally, add table to the bottom of the plot. ----
  ############################################################# |
  if (!is.null(table)) {
    df_stats_table <- df_grp %>%
      dplyr::summarise(
        h_format_row(
          x = sfun(.data[[y]], ...)[table],
          format = table_format,
          labels = table_labels
        ),
        .groups = "drop"
      )

    stats_lev <- rev(setdiff(colnames(df_stats_table), c(group_var, x)))

    df_stats_table <- df_stats_table %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(c(group_var, x)),
        names_to = "stat",
        values_to = "value",
        names_ptypes = list(stat = factor(levels = stats_lev))
      )

    tbl <- ggplot2::ggplot(
      df_stats_table,
      ggplot2::aes(x = .data[[x]], y = .data[["stat"]], label = .data[["value"]])
    ) +
      ggplot2::geom_text(size = table_font_size) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = table_font_size * ggplot2::.pt,
          margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 5)
        ),
        strip.text = ggplot2::element_text(hjust = 0),
        strip.text.x = ggplot2::element_text(
          size = table_font_size * ggplot2::.pt,
          margin = ggplot2::margin(1.5, 0, 1.5, 0, "pt")
        ),
        strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
        legend.position = "none"
      )

    if (!is.null(group_var)) {
      tbl <- tbl + ggplot2::facet_wrap(facets = group_var, ncol = 1)
    }

    if (!as_list) {
      # align plot and table
      cowplot::plot_grid(
        p,
        tbl,
        ncol = 1,
        align = "v",
        axis = "tblr",
        rel_heights = c(rel_height_plot, 1 - rel_height_plot)
      )
    } else {
      list(plot = p, table = tbl)
    }
  } else {
    p
  }
}

#' Helper function to format the optional `g_lineplot` table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (named `list`)\cr list of numerical values to be formatted and optionally labeled.
#'   Elements of `x` must be `numeric` vectors.
#' @param format (named `character` or `NULL`)\cr format patterns for `x`. Names of the `format` must
#'   match the names of `x`. This parameter is passed directly to the `rtables::format_rcell`
#'   function through the `format` parameter.
#' @param labels (named `character` or `NULL`)\cr optional labels for `x`. Names of the `labels` must
#'   match the names of `x`. When a label is not specified for an element of `x`,
#'   then this function tries to use `label` or `names` (in this order) attribute of that element
#'   (depending on which one exists and it is not `NULL` or `NA` or `NaN`). If none of these attributes
#'   are attached to a given element of `x`, then the label is automatically generated.
#'
#' @return A single row `data.frame` object.
#'
#' @examples
#' mean_ci <- c(48, 51)
#' x <- list(mean = 50, mean_ci = mean_ci)
#' format <- c(mean = "xx.x", mean_ci = "(xx.xx, xx.xx)")
#' labels <- c(mean = "My Mean")
#' h_format_row(x, format, labels)
#'
#' attr(mean_ci, "label") <- "Mean 95% CI"
#' x <- list(mean = 50, mean_ci = mean_ci)
#' h_format_row(x, format, labels)
#'
#' @export
h_format_row <- function(x, format, labels = NULL) {
  # cell: one row, one column data.frame
  format_cell <- function(x, format, label = NULL) {
    fc <- format_rcell(x = x, format = format)
    if (is.na(fc)) {
      fc <- "NA"
    }
    x_label <- attr(x, "label")
    if (!is.null(label) && !is.na(label)) {
      names(fc) <- label
    } else if (!is.null(x_label) && !is.na(x_label)) {
      names(fc) <- x_label
    } else if (length(x) == length(fc)) {
      names(fc) <- names(x)
    }
    as.data.frame(t(fc))
  }

  row <- do.call(
    cbind,
    lapply(
      names(x), function(xn) format_cell(x[[xn]], format = format[[xn]], label = labels[xn])
    )
  )

  row
}

#' Control function for `g_lineplot()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Default values for `variables` parameter in `g_lineplot` function.
#' A variable's default value can be overwritten for any variable.
#'
#' @param x (`string`)\cr x-variable name.
#' @param y (`string`)\cr y-variable name.
#' @param group_var (`string` or `NA`)\cr group variable name.
#' @param subject_var (`string` or `NA`)\cr subject variable name.
#' @param facet_var (`string` or `NA`)\cr faceting variable name.
#' @param paramcd (`string` or `NA`)\cr parameter code variable name.
#' @param y_unit (`string` or `NA`)\cr y-axis unit variable name.
#'
#' @return A named character vector of variable names.
#'
#' @examples
#' control_lineplot_vars()
#' control_lineplot_vars(group_var = NA)
#'
#' @export
control_lineplot_vars <- function(x = "AVISIT",
                                  y = "AVAL",
                                  group_var = "ARM",
                                  facet_var = NA,
                                  paramcd = "PARAMCD",
                                  y_unit = "AVALU",
                                  subject_var = "USUBJID") {
  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(group_var, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(facet_var, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(subject_var, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(paramcd, na.ok = TRUE, null.ok = TRUE)
  checkmate::assert_string(y_unit, na.ok = TRUE, null.ok = TRUE)

  variables <- c(
    x = x, y = y, group_var = group_var, paramcd = paramcd,
    y_unit = y_unit, subject_var = subject_var, facet_var = facet_var
  )
  return(variables)
}
