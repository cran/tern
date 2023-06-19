## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
library(formatters)
library(tern)
library(dplyr)

## -----------------------------------------------------------------------------
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)

df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = format_fraction)
  ) %>%
  build_table(df2)

## -----------------------------------------------------------------------------
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)

df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = format_fraction)
  ) %>%
  build_table(df2)

## -----------------------------------------------------------------------------
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)
df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = "xx / xx")
  ) %>%
  build_table(df2)

## -----------------------------------------------------------------------------
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)
df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = "xx.x / xx.x")
  ) %>%
  build_table(df2)

## -----------------------------------------------------------------------------
format_fraction_fixed_dp(x = c(num = 1L, denom = 3L))
format_fraction_fixed_dp(x = c(num = 1L, denom = 2L))

format_count_fraction_fixed_dp(x = c(2, 0.6667))
format_count_fraction_fixed_dp(x = c(2, 0.25))

## -----------------------------------------------------------------------------
extreme_format <- format_extreme_values(digits = 2)
extreme_format(0.235)
extreme_format(0.001)
extreme_format(Inf)

## -----------------------------------------------------------------------------
fraction_format <- format_fraction_threshold(0.05)
fraction_format(x = c(20, 0.1))
fraction_format(x = c(2, 0.01))

## -----------------------------------------------------------------------------
# First we will see how the format_fraction_fixed_dp code works and displays the outputs
format_fraction_fixed_dp <- function(x, ...) {
  attr(x, "label") <- NULL
  checkmate::assert_vector(x)
  checkmate::assert_count(x["num"])
  checkmate::assert_count(x["denom"])

  result <- if (x["num"] == 0) {
    paste0(x["num"], "/", x["denom"])
  } else {
    paste0(
      x["num"], "/", x["denom"],
      " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
    )
  }
  return(result)
}

## -----------------------------------------------------------------------------
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
) %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = format_fraction_fixed_dp)
  ) %>%
  build_table(df2)

## -----------------------------------------------------------------------------
custom_format <- function(x, ...) {
  attr(x, "label") <- NULL
  checkmate::assert_vector(x)
  checkmate::assert_count(x["num"])
  checkmate::assert_count(x["denom"])

  result <- if (x["num"] == 0) {
    paste0(x["num"]) # We remove the denominator on this line so that only a 0 is displayed
  } else {
    paste0(
      x["num"], "/", x["denom"],
      " (", sprintf("%.3f", round(x["num"] / x["denom"] * 100, 1)), "%)" # We include 3 decimal places with %.3f
    )
  }
  return(result)
}

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = custom_format) # Here we implement our new custom_format function
  ) %>%
  build_table(df2)

