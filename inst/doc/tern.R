## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(tern)
library(dplyr)

## -----------------------------------------------------------------------------
# Create table layout pure rtables
lyt <- rtables::basic_table() %>%
  rtables::split_cols_by(var = "ARM") %>%
  rtables::split_rows_by(var = "AVISIT") %>%
  rtables::analyze(vars = "AVAL", mean, format = "xx.x")

## -----------------------------------------------------------------------------
# Create table layout with tern analyze_vars analyze function
lyt2 <- rtables::basic_table() %>%
  rtables::split_cols_by(var = "ARM") %>%
  rtables::split_rows_by(var = "AVISIT") %>%
  analyze_vars(vars = "AVAL", .formats = c("mean_sd" = "(xx.xx, xx.xx)"))

## -----------------------------------------------------------------------------
# Apply table layout to data and produce `rtables` object

adrs <- formatters::ex_adrs

rtables::build_table(lyt, df = adrs)
rtables::build_table(lyt2, df = adrs)

## -----------------------------------------------------------------------------
adsl <- formatters::ex_adsl
adlb <- formatters::ex_adlb
adlb <- dplyr::filter(adlb, PARAMCD == "ALT", AVISIT != "SCREENING")

## -----------------------------------------------------------------------------
library(nestcolor)

## ----fig.alt='Basic line plot'------------------------------------------------
# Mean with CI
g_lineplot(adlb, adsl, subtitle = "Laboratory Test:")

## ----fig.height=10, fig.width=8, fig.alt='Line plot with table'---------------
# Mean with CI, table, and customized confidence level
g_lineplot(
  adlb,
  adsl,
  table = c("n", "mean", "mean_ci"),
  title = "Plot of Mean and 80% Confidence Limits by Visit"
)

