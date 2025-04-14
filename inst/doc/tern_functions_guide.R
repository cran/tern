## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(dplyr)
library(tern)

## Fabricate dataset
dta_test <- data.frame(
  USUBJID = rep(1:6, each = 3),
  AVISIT = rep(paste0("V", 1:3), 6),
  ARM = rep(LETTERS[1:3], rep(6, 3)),
  AVAL = c(9:1, rep(NA, 9))
) %>%
  mutate(ABLFLL = AVISIT == "V1") %>%
  group_by(USUBJID) %>%
  mutate(
    BLVAL = AVAL[ABLFLL],
    CHG = AVAL - BLVAL
  ) %>%
  ungroup()

## -----------------------------------------------------------------------------
fix_layout <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT")

# Dealing with NAs: na_rm = TRUE
fix_layout %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL")) %>%
  build_table(dta_test) %>%
  print()

# Dealing with NAs: na_rm = FALSE
fix_layout %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL"), na_rm = FALSE) %>%
  build_table(dta_test) %>%
  print()

# changing the NA string (it is done on all levels)
fix_layout %>%
  summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL"), na_str = "my_na") %>%
  build_table(dta_test) %>%
  print()

## -----------------------------------------------------------------------------
# changing n count format and label and indentation
fix_layout %>%
  summarize_change("CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
    .stats = c("n", "mean"), # reducing the number of stats for visual appreciation
    .formats = c(n = "xx.xx"),
    .labels = c(n = "NnNn"),
    .indent_mods = c(n = 5), na_str = "nA"
  ) %>%
  build_table(dta_test) %>%
  print()

## -----------------------------------------------------------------------------
# changing n count format and label and indentation
fix_layout %>%
  summarize_change("CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
    .stats = c("n", "mean"), # reducing the number of stats for visual appreciation
    .formats = c(n = function(x, ...) as.character(x * 100))
  ) %>% # Note you need ...!!!
  build_table(dta_test) %>%
  print()

## -----------------------------------------------------------------------------
# changing n count format and label and indentation
fix_layout %>%
  summarize_change(
    "CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
    .stats = c("n", "my_stat" = function(df, ...) {
      a <- mean(df$AVAL, na.rm = TRUE)
      b <- list(...)$.N_row # It has access at all `?rtables::additional_fun_params`
      a / b
    }),
    .formats = c("my_stat" = function(x, ...) sprintf("%.2f", x))
  ) %>%
  build_table(dta_test)

