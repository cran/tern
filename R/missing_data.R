#' Missing data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Substitute missing data with a string or factor level.
#'
#' @param x (`factor` or `character`)\cr values for which any missing values should be substituted.
#' @param label (`string`)\cr string that missing data should be replaced with.
#'
#' @return `x` with any `NA` values substituted by `label`.
#'
#' @examples
#' explicit_na(c(NA, "a", "b"))
#' is.na(explicit_na(c(NA, "a", "b")))
#'
#' explicit_na(factor(c(NA, "a", "b")))
#' is.na(explicit_na(factor(c(NA, "a", "b"))))
#'
#' explicit_na(sas_na(c("a", "")))
#'
#' @export
explicit_na <- function(x, label = "<Missing>") {
  checkmate::assert_string(label)

  if (is.factor(x)) {
    x <- forcats::fct_na_value_to_level(x, label)
    forcats::fct_drop(x, only = label)
  } else if (is.character(x)) {
    x[is.na(x)] <- label
    x
  } else {
    stop("only factors and character vectors allowed")
  }
}

#' Convert strings to `NA`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' SAS imports missing data as empty strings or strings with whitespaces only. This helper function can be used to
#' convert these values to `NA`s.
#'
#' @inheritParams explicit_na
#' @param empty (`flag`)\cr if `TRUE`, empty strings get replaced by `NA`.
#' @param whitespaces (`flag`)\cr if `TRUE`, strings made from only whitespaces get replaced with `NA`.
#'
#' @return `x` with `""` and/or whitespace-only values substituted by `NA`, depending on the values of
#'   `empty` and `whitespaces`.
#'
#' @examples
#' sas_na(c("1", "", " ", "   ", "b"))
#' sas_na(factor(c("", " ", "b")))
#'
#' is.na(sas_na(c("1", "", " ", "   ", "b")))
#'
#' @export
sas_na <- function(x, empty = TRUE, whitespaces = TRUE) {
  checkmate::assert_flag(empty)
  checkmate::assert_flag(whitespaces)

  if (is.factor(x)) {
    empty_levels <- levels(x) == ""
    if (empty && any(empty_levels)) levels(x)[empty_levels] <- NA

    ws_levels <- grepl("^\\s+$", levels(x))
    if (whitespaces && any(ws_levels)) levels(x)[ws_levels] <- NA

    x
  } else if (is.character(x)) {
    if (empty) x[x == ""] <- NA_character_

    if (whitespaces) x[grepl("^\\s+$", x)] <- NA_character_

    x
  } else {
    stop("only factors and character vectors allowed")
  }
}
