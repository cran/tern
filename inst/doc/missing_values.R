## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(rtables)
library(formatters)
library(tern)
library(dplyr)

## -----------------------------------------------------------------------------
adsl <- tern_ex_adsl
adsl$SEX <- as.factor(adsl$SEX)

vars <- c("AGE", "SEX", "RACE", "BMRKR1")
var_labels <- c(
  "Age (yr)",
  "Sex",
  "Race",
  "Continous Level Biomarker 1"
)

result <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)
result

## -----------------------------------------------------------------------------
adsl <- tern_ex_adsl
adsl$SEX[adsl$SEX == "M"] <- NA
adsl <- df_explicit_na(adsl)

vars <- c("AGE", "SEX")
var_labels <- c(
  "Age (yr)",
  "Sex"
)

result <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)
result

## -----------------------------------------------------------------------------
adsl <- tern_ex_adsl
adsl$SEX[adsl$SEX == "M"] <- NA
adsl <- df_explicit_na(adsl, na_level = "Missing Values")

result <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)
result

## -----------------------------------------------------------------------------
adsl <- tern_ex_adsl
adsl$AGE[adsl$AGE < 30] <- NA
adsl <- df_explicit_na(adsl)

vars <- c("AGE", "SEX")
var_labels <- c(
  "Age (yr)",
  "Sex"
)

result <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  add_overall_col("All Patients") %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels
  ) %>%
  build_table(adsl)
result

