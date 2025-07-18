#' Helper function for deriving analysis datasets for select laboratory tables
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function that merges ADSL and ADLB datasets so that missing lab test records are inserted in the
#' output dataset. Remember that `na_level` must match the needed pre-processing
#' done with [df_explicit_na()] to have the desired output.
#'
#' @param adsl (`data.frame`)\cr ADSL data frame.
#' @param adlb (`data.frame`)\cr ADLB data frame.
#' @param worst_flag (named `character`)\cr worst post-baseline lab flag variable. See how this is implemented in the
#'   following examples.
#' @param by_visit (`flag`)\cr defaults to `FALSE` to generate worst grade per patient.
#'   If worst grade per patient per visit is specified for `worst_flag`, then
#'   `by_visit` should be `TRUE` to generate worst grade patient per visit.
#' @param no_fillin_visits (named `character`)\cr visits that are not considered for post-baseline worst toxicity
#'   grade. Defaults to `c("SCREENING", "BASELINE")`.
#'
#' @return `df` containing variables shared between `adlb` and `adsl` along with variables `PARAM`, `PARAMCD`,
#'   `ATOXGR`, and `BTOXGR` relevant for analysis. Optionally, `AVISIT` are `AVISITN` are included when
#'   `by_visit = TRUE` and `no_fillin_visits = c("SCREENING", "BASELINE")`.
#'
#' @details In the result data missing records will be created for the following situations:
#'   * Patients who are present in `adsl` but have no lab data in `adlb` (both baseline and post-baseline).
#'   * Patients who do not have any post-baseline lab values.
#'   * Patients without any post-baseline values flagged as the worst.
#'
#' @examples
#' # `h_adsl_adlb_merge_using_worst_flag`
#' adlb_out <- h_adsl_adlb_merge_using_worst_flag(
#'   tern_ex_adsl,
#'   tern_ex_adlb,
#'   worst_flag = c("WGRHIFL" = "Y")
#' )
#'
#' # `h_adsl_adlb_merge_using_worst_flag` by visit example
#' adlb_out_by_visit <- h_adsl_adlb_merge_using_worst_flag(
#'   tern_ex_adsl,
#'   tern_ex_adlb,
#'   worst_flag = c("WGRLOVFL" = "Y"),
#'   by_visit = TRUE
#' )
#'
#' @export
h_adsl_adlb_merge_using_worst_flag <- function(adsl,
                                               adlb,
                                               worst_flag = c("WGRHIFL" = "Y"),
                                               by_visit = FALSE,
                                               no_fillin_visits = c("SCREENING", "BASELINE")) {
  col_names <- names(worst_flag)
  filter_values <- worst_flag

  temp <- Map(
    function(x, y) which(adlb[[x]] == y),
    col_names,
    filter_values
  )

  position_satisfy_filters <- Reduce(intersect, temp)

  adsl_adlb_common_columns <- intersect(colnames(adsl), colnames(adlb))
  columns_from_adlb <- c("USUBJID", "PARAM", "PARAMCD", "AVISIT", "AVISITN", "ATOXGR", "BTOXGR")

  adlb_f <- adlb[position_satisfy_filters, ] %>%
    dplyr::filter(!.data[["AVISIT"]] %in% no_fillin_visits)
  adlb_f <- adlb_f[, columns_from_adlb]

  avisits_grid <- adlb %>%
    dplyr::filter(!.data[["AVISIT"]] %in% no_fillin_visits) %>%
    dplyr::pull(.data[["AVISIT"]]) %>%
    unique()

  if (by_visit) {
    adsl_lb <- expand.grid(
      USUBJID = unique(adsl$USUBJID),
      AVISIT = avisits_grid,
      PARAMCD = unique(adlb$PARAMCD)
    )

    adsl_lb <- adsl_lb %>%
      dplyr::left_join(unique(adlb[c("AVISIT", "AVISITN")]), by = "AVISIT") %>%
      dplyr::left_join(unique(adlb[c("PARAM", "PARAMCD")]), by = "PARAMCD")

    adsl1 <- adsl[, adsl_adlb_common_columns]
    adsl_lb <- adsl1 %>% merge(adsl_lb, by = "USUBJID")

    by_variables_from_adlb <- c("USUBJID", "AVISIT", "AVISITN", "PARAMCD", "PARAM")

    adlb_btoxgr <- adlb %>%
      dplyr::select(c("USUBJID", "PARAMCD", "BTOXGR")) %>%
      unique() %>%
      dplyr::rename("BTOXGR_MAP" = "BTOXGR")

    adlb_out <- merge(
      adlb_f,
      adsl_lb,
      by = by_variables_from_adlb,
      all = TRUE,
      sort = FALSE
    )
    adlb_out <- adlb_out %>%
      dplyr::left_join(adlb_btoxgr, by = c("USUBJID", "PARAMCD")) %>%
      dplyr::mutate(BTOXGR = .data$BTOXGR_MAP) %>%
      dplyr::select(-"BTOXGR_MAP")

    adlb_var_labels <- c(
      formatters::var_labels(adlb[by_variables_from_adlb]),
      formatters::var_labels(adlb[columns_from_adlb[!columns_from_adlb %in% by_variables_from_adlb]]),
      formatters::var_labels(adsl[adsl_adlb_common_columns[adsl_adlb_common_columns != "USUBJID"]])
    )
  } else {
    adsl_lb <- expand.grid(
      USUBJID = unique(adsl$USUBJID),
      PARAMCD = unique(adlb$PARAMCD)
    )

    adsl_lb <- adsl_lb %>% dplyr::left_join(unique(adlb[c("PARAM", "PARAMCD")]), by = "PARAMCD")

    adsl1 <- adsl[, adsl_adlb_common_columns]
    adsl_lb <- adsl1 %>% merge(adsl_lb, by = "USUBJID")

    by_variables_from_adlb <- c("USUBJID", "PARAMCD", "PARAM")

    adlb_out <- merge(
      adlb_f,
      adsl_lb,
      by = by_variables_from_adlb,
      all = TRUE,
      sort = FALSE
    )

    adlb_var_labels <- c(
      formatters::var_labels(adlb[by_variables_from_adlb]),
      formatters::var_labels(adlb[columns_from_adlb[!columns_from_adlb %in% by_variables_from_adlb]]),
      formatters::var_labels(adsl[adsl_adlb_common_columns[adsl_adlb_common_columns != "USUBJID"]])
    )
  }

  adlb_out$ATOXGR <- as.factor(adlb_out$ATOXGR)
  adlb_out$BTOXGR <- as.factor(adlb_out$BTOXGR)

  formatters::var_labels(adlb_out) <- adlb_var_labels

  adlb_out
}
