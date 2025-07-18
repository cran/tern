#' Encode categorical missing values in a data frame
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a helper function to encode missing entries across groups of categorical
#' variables in a data frame.
#'
#' @details Missing entries are those with `NA` or empty strings and will
#'   be replaced with a specified value. If factor variables include missing
#'   values, the missing value will be inserted as the last level.
#'   Similarly, in case character or logical variables should be converted to factors
#'   with the `char_as_factor` or `logical_as_factor` options, the missing values will
#'   be set as the last level.
#'
#' @param data (`data.frame`)\cr data set.
#' @param omit_columns (`character`)\cr names of variables from `data` that should
#'   not be modified by this function.
#' @param char_as_factor (`flag`)\cr whether to convert character variables
#'   in `data` to factors.
#' @param logical_as_factor (`flag`)\cr whether to convert logical variables
#'   in `data` to factors.
#' @param na_level (`string`)\cr string used to replace all `NA` or empty
#'   values inside non-`omit_columns` columns.
#'
#' @return A `data.frame` with the chosen modifications applied.
#'
#' @seealso [sas_na()] and [explicit_na()] for other missing data helper functions.
#'
#' @examples
#' my_data <- data.frame(
#'   u = c(TRUE, FALSE, NA, TRUE),
#'   v = factor(c("A", NA, NA, NA), levels = c("Z", "A")),
#'   w = c("A", "B", NA, "C"),
#'   x = c("D", "E", "F", NA),
#'   y = c("G", "H", "I", ""),
#'   z = c(1, 2, 3, 4),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example 1
#' # Encode missing values in all character or factor columns.
#' df_explicit_na(my_data)
#' # Also convert logical columns to factor columns.
#' df_explicit_na(my_data, logical_as_factor = TRUE)
#' # Encode missing values in a subset of columns.
#' df_explicit_na(my_data, omit_columns = c("x", "y"))
#'
#' # Example 2
#' # Here we purposefully convert all `M` values to `NA` in the `SEX` variable.
#' # After running `df_explicit_na` the `NA` values are encoded as `<Missing>` but they are not
#' # included when generating `rtables`.
#' adsl <- tern_ex_adsl
#' adsl$SEX[adsl$SEX == "M"] <- NA
#' adsl <- df_explicit_na(adsl)
#'
#' # If you want the `Na` values to be displayed in the table use the `na_level` argument.
#' adsl <- tern_ex_adsl
#' adsl$SEX[adsl$SEX == "M"] <- NA
#' adsl <- df_explicit_na(adsl, na_level = "Missing Values")
#'
#' # Example 3
#' # Numeric variables that have missing values are not altered. This means that any `NA` value in
#' # a numeric variable will not be included in the summary statistics, nor will they be included
#' # in the denominator value for calculating the percent values.
#' adsl <- tern_ex_adsl
#' adsl$AGE[adsl$AGE < 30] <- NA
#' adsl <- df_explicit_na(adsl)
#'
#' @export
df_explicit_na <- function(data,
                           omit_columns = NULL,
                           char_as_factor = TRUE,
                           logical_as_factor = FALSE,
                           na_level = "<Missing>") {
  checkmate::assert_character(omit_columns, null.ok = TRUE, min.len = 1, any.missing = FALSE)
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(char_as_factor)
  checkmate::assert_flag(logical_as_factor)
  checkmate::assert_string(na_level)

  target_vars <- if (is.null(omit_columns)) {
    names(data)
  } else {
    setdiff(names(data), omit_columns) # May have duplicates.
  }
  if (length(target_vars) == 0) {
    return(data)
  }

  l_target_vars <- split(target_vars, target_vars)

  # Makes sure target_vars exist in data and names are not duplicated.
  assert_df_with_variables(data, l_target_vars)

  for (x in target_vars) {
    xi <- data[[x]]
    xi_label <- obj_label(xi)

    # Determine whether to convert character or logical input.
    do_char_conversion <- is.character(xi) && char_as_factor
    do_logical_conversion <- is.logical(xi) && logical_as_factor

    # Pre-convert logical to character to deal correctly with replacing NA
    # values below.
    if (do_logical_conversion) {
      xi <- as.character(xi)
    }

    if (is.factor(xi) || is.character(xi)) {
      # Handle empty strings and NA values.
      xi <- explicit_na(sas_na(xi), label = na_level)

      # Convert to factors if requested for the original type,
      # set na_level as the last value.
      if (do_char_conversion || do_logical_conversion) {
        levels_xi <- setdiff(sort(unique(xi)), na_level)
        if (na_level %in% unique(xi)) {
          levels_xi <- c(levels_xi, na_level)
        }

        xi <- factor(xi, levels = levels_xi)
      }

      data[, x] <- formatters::with_label(xi, label = xi_label)
    }
  }
  return(data)
}
