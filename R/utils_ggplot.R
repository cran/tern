#' Convert `rtable` objects to `ggplot` objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Given a [rtables::rtable()] object, performs basic conversion to a [ggplot2::ggplot()] object built using
#' functions from the `ggplot2` package. Any table titles and/or footnotes are ignored.
#'
#' @param tbl (`VTableTree`)\cr `rtables` table object.
#' @param fontsize (`numeric(1)`)\cr font size.
#' @param colwidths (`numeric` or `NULL`)\cr a vector of column widths. Each element's position in
#'   `colwidths` corresponds to the column of `tbl` in the same position. If `NULL`, column widths
#'   are calculated according to maximum number of characters per column.
#' @param lbl_col_padding (`numeric`)\cr additional padding to use when calculating spacing between
#'   the first (label) column and the second column of `tbl`. If `colwidths` is specified,
#'   the width of the first column becomes `colwidths[1] + lbl_col_padding`. Defaults to 0.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' dta <- data.frame(
#'   ARM     = rep(LETTERS[1:3], rep(6, 3)),
#'   AVISIT  = rep(paste0("V", 1:3), 6),
#'   AVAL    = c(9:1, rep(NA, 9))
#' )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   split_rows_by(var = "AVISIT") %>%
#'   analyze_vars(vars = "AVAL")
#'
#' tbl <- build_table(lyt, df = dta)
#'
#' rtable2gg(tbl)
#'
#' rtable2gg(tbl, fontsize = 15, colwidths = c(2, 1, 1, 1))
#'
#' @export
rtable2gg <- function(tbl, fontsize = 12, colwidths = NULL, lbl_col_padding = 0) {
  mat <- rtables::matrix_form(tbl, indent_rownames = TRUE)
  mat_strings <- formatters::mf_strings(mat)
  mat_aligns <- formatters::mf_aligns(mat)
  mat_indent <- formatters::mf_rinfo(mat)$indent
  mat_display <- formatters::mf_display(mat)
  nlines_hdr <- formatters::mf_nlheader(mat)
  shared_hdr_rows <- which(apply(mat_display, 1, function(x) (any(!x))))

  tbl_df <- data.frame(mat_strings)
  body_rows <- seq(nlines_hdr + 1, nrow(tbl_df))
  mat_aligns <- apply(mat_aligns, 1:2, function(x) if (x == "left") 0 else if (x == "right") 1 else 0.5)

  # Apply indentation in first column
  tbl_df[body_rows, 1] <- sapply(body_rows, function(i) {
    ind_i <- mat_indent[i - nlines_hdr] * 4
    if (ind_i > 0) paste0(paste(rep(" ", ind_i), collapse = ""), tbl_df[i, 1]) else tbl_df[i, 1]
  })

  # Get column widths
  if (is.null(colwidths)) {
    colwidths <- apply(tbl_df, 2, function(x) max(nchar(x))) + 1
  }
  tot_width <- sum(colwidths) + lbl_col_padding

  if (length(shared_hdr_rows) > 0) {
    tbl_df <- tbl_df[-shared_hdr_rows, ]
    mat_aligns <- mat_aligns[-shared_hdr_rows, ]
  }

  res <- ggplot(data = tbl_df) +
    theme_void() +
    scale_x_continuous(limits = c(0, tot_width)) +
    scale_y_continuous(limits = c(0, nrow(mat_strings))) +
    annotate(
      "segment",
      x = 0, xend = tot_width,
      y = nrow(mat_strings) - nlines_hdr + 0.5, yend = nrow(mat_strings) - nlines_hdr + 0.5
    )

  # If header content spans multiple columns, center over these columns
  if (length(shared_hdr_rows) > 0) {
    mat_strings[shared_hdr_rows, ] <- trimws(mat_strings[shared_hdr_rows, ])
    for (hr in shared_hdr_rows) {
      hdr_lbls <- mat_strings[1:hr, mat_display[hr, -1]]
      hdr_lbls <- matrix(hdr_lbls[nzchar(hdr_lbls)], nrow = hr)
      for (idx_hl in seq_len(ncol(hdr_lbls))) {
        cur_lbl <- tail(hdr_lbls[, idx_hl], 1)
        which_cols <- if (hr == 1) {
          which(mat_strings[hr, ] == hdr_lbls[idx_hl])
        } else { # for >2 col splits, only print labels for each unique combo of nested columns
          which(
            apply(mat_strings[1:hr, ], 2, function(x) all(x == hdr_lbls[1:hr, idx_hl]))
          )
        }
        line_pos <- c(
          sum(colwidths[1:(which_cols[1] - 1)]) + 1 + lbl_col_padding,
          sum(colwidths[1:max(which_cols)]) - 1 + lbl_col_padding
        )

        res <- res +
          annotate(
            "text",
            x = mean(line_pos),
            y = nrow(mat_strings) + 1 - hr,
            label = cur_lbl,
            size = fontsize / .pt
          ) +
          annotate(
            "segment",
            x = line_pos[1],
            xend = line_pos[2],
            y = nrow(mat_strings) - hr + 0.5,
            yend = nrow(mat_strings) - hr + 0.5
          )
      }
    }
  }

  # Add table columns
  for (i in seq_len(ncol(tbl_df))) {
    res <- res + annotate(
      "text",
      x = if (i == 1) 0 else sum(colwidths[1:i]) - 0.5 * colwidths[i] + lbl_col_padding,
      y = rev(seq_len(nrow(tbl_df))),
      label = tbl_df[, i],
      hjust = mat_aligns[, i],
      size = fontsize / .pt
    )
  }

  res
}

#' Convert `data.frame` object to `ggplot` object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Given a `data.frame` object, performs basic conversion to a [ggplot2::ggplot()] object built using
#' functions from the `ggplot2` package.
#'
#' @param df (`data.frame`)\cr a data frame.
#' @param colwidths (`numeric` or `NULL`)\cr a vector of column widths. Each element's position in
#'   `colwidths` corresponds to the column of `df` in the same position. If `NULL`, column widths
#'   are calculated according to maximum number of characters per column.
#' @param font_size (`numeric(1)`)\cr font size.
#' @param col_labels (`flag`)\cr whether the column names (labels) of `df` should be used as the first row
#'   of the output table.
#' @param col_lab_fontface (`string`)\cr font face to apply to the first row (of column labels
#'   if `col_labels = TRUE`). Defaults to `"bold"`.
#' @param hline (`flag`)\cr whether a horizontal line should be printed below the first row of the table.
#' @param bg_fill (`string`)\cr table background fill color.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' df2gg(head(iris, 5))
#'
#' df2gg(head(iris, 5), font_size = 15, colwidths = c(1, 1, 1, 1, 1))
#' }
#' @keywords internal
df2gg <- function(df,
                  colwidths = NULL,
                  font_size = 10,
                  col_labels = TRUE,
                  col_lab_fontface = "bold",
                  hline = TRUE,
                  bg_fill = NULL) {
  # convert to text
  df <- as.data.frame(apply(df, 1:2, function(x) if (is.na(x)) "NA" else as.character(x)))

  if (col_labels) {
    df <- as.matrix(df)
    df <- rbind(colnames(df), df)
  }

  # Get column widths
  if (is.null(colwidths)) {
    colwidths <- apply(df, 2, function(x) max(nchar(x), na.rm = TRUE))
  }
  tot_width <- sum(colwidths)

  res <- ggplot(data = df) +
    theme_void() +
    scale_x_continuous(limits = c(0, tot_width)) +
    scale_y_continuous(limits = c(1, nrow(df)))

  if (!is.null(bg_fill)) res <- res + theme(plot.background = element_rect(fill = bg_fill))

  if (hline) {
    res <- res +
      annotate(
        "segment",
        x = 0 + 0.2 * colwidths[2], xend = tot_width - 0.1 * tail(colwidths, 1),
        y = nrow(df) - 0.5, yend = nrow(df) - 0.5
      )
  }

  for (i in seq_len(ncol(df))) {
    line_pos <- c(
      if (i == 1) 0 else sum(colwidths[1:(i - 1)]),
      sum(colwidths[1:i])
    )
    res <- res +
      annotate(
        "text",
        x = mean(line_pos),
        y = rev(seq_len(nrow(df))),
        label = df[, i],
        size = font_size / .pt,
        fontface = if (col_labels) {
          c(col_lab_fontface, rep("plain", nrow(df) - 1))
        } else {
          rep("plain", nrow(df))
        }
      )
  }

  res
}
