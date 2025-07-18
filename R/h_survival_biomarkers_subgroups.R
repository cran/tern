#' Helper functions for tabulating biomarker effects on survival by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions which are documented here separately to not confuse the user
#' when reading about the user-facing functions.
#'
#' @inheritParams survival_biomarkers_subgroups
#' @inheritParams argument_convention
#' @inheritParams fit_coxreg_multivar
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#'
#' adtte <- tern_ex_adtte
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- formatters::var_labels(adtte, fill = FALSE)
#'
#' adtte_f <- adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVALU = as.character(AVALU),
#'     is_event = CNSR == 0
#'   )
#' labels <- c("AVALU" = adtte_labels[["AVALU"]], "is_event" = "Event Flag")
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
#'
#' @name h_survival_biomarkers_subgroups
NULL

#' @describeIn h_survival_biomarkers_subgroups Helps with converting the "survival" function variable list
#'   to the "Cox regression" variable list. The reason is that currently there is an inconsistency between the variable
#'   names accepted by `extract_survival_subgroups()` and `fit_coxreg_multivar()`.
#'
#' @param biomarker (`string`)\cr the name of the biomarker variable.
#'
#' @return
#' * `h_surv_to_coxreg_variables()` returns a named `list` of elements `time`, `event`, `arm`,
#'   `covariates`, and `strata`.
#'
#' @examples
#' # This is how the variable list is converted internally.
#' h_surv_to_coxreg_variables(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "EVNT",
#'     covariates = c("A", "B"),
#'     strata = "D"
#'   ),
#'   biomarker = "AGE"
#' )
#'
#' @export
h_surv_to_coxreg_variables <- function(variables, biomarker) {
  checkmate::assert_list(variables)
  checkmate::assert_string(variables$tte)
  checkmate::assert_string(variables$is_event)
  checkmate::assert_string(biomarker)
  list(
    time = variables$tte,
    event = variables$is_event,
    arm = biomarker,
    covariates = variables$covariates,
    strata = variables$strata
  )
}

#' @describeIn h_survival_biomarkers_subgroups Prepares estimates for number of events, patients and median survival
#'   times, as well as hazard ratio estimates, confidence intervals and p-values, for multiple biomarkers
#'   in a given single data set.
#'   `variables` corresponds to names of variables found in `data`, passed as a named list and requires elements
#'   `tte`, `is_event`, `biomarkers` (vector of continuous biomarker variables) and optionally `subgroups` and `strata`.
#'
#' @return
#' * `h_coxreg_mult_cont_df()` returns a `data.frame` containing estimates and statistics for the selected biomarkers.
#'
#' @examples
#' # For a single population, estimate separately the effects
#' # of two biomarkers.
#' df <- h_coxreg_mult_cont_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     strata = c("STRATA1", "STRATA2")
#'   ),
#'   data = adtte_f
#' )
#' df
#'
#' # If the data set is empty, still the corresponding rows with missings are returned.
#' h_coxreg_mult_cont_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "REGION1",
#'     strata = c("STRATA1", "STRATA2")
#'   ),
#'   data = adtte_f[NULL, ]
#' )
#'
#' @export
h_coxreg_mult_cont_df <- function(variables,
                                  data,
                                  control = control_coxreg()) {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `h_coxreg_mult_cont_df() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

  assert_df_with_variables(data, variables)
  checkmate::assert_list(control, names = "named")
  checkmate::assert_character(variables$biomarkers, min.len = 1, any.missing = FALSE)
  conf_level <- control[["conf_level"]]
  pval_label <- paste0(
    # the regex capitalizes the first letter of the string / senetence.
    "p-value (", gsub("(^[a-z])", "\\U\\1", trimws(control[["pval_method"]]), perl = TRUE), ")"
  )
  # If there is any data, run model, otherwise return empty results.
  if (nrow(data) > 0) {
    bm_cols <- match(variables$biomarkers, names(data))
    l_result <- lapply(variables$biomarkers, function(bm) {
      coxreg_list <- fit_coxreg_multivar(
        variables = h_surv_to_coxreg_variables(variables, bm),
        data = data,
        control = control
      )
      result <- do.call(
        h_coxreg_multivar_extract,
        c(list(var = bm), coxreg_list[c("mod", "data", "control")])
      )
      data_fit <- as.data.frame(as.matrix(coxreg_list$mod$y))
      data_fit$status <- as.logical(data_fit$status)
      median <- s_surv_time(
        df = data_fit,
        .var = "time",
        is_event = "status"
      )$median
      data.frame(
        # Dummy column needed downstream to create a nested header.
        biomarker = bm,
        biomarker_label = formatters::var_labels(data[bm], fill = TRUE),
        n_tot = coxreg_list$mod$n,
        n_tot_events = coxreg_list$mod$nevent,
        median = as.numeric(median),
        result[1L, c("hr", "lcl", "ucl")],
        conf_level = conf_level,
        pval = result[1L, "pval"],
        pval_label = pval_label,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, args = c(l_result, make.row.names = FALSE))
  } else {
    data.frame(
      biomarker = variables$biomarkers,
      biomarker_label = formatters::var_labels(data[variables$biomarkers], fill = TRUE),
      n_tot = 0L,
      n_tot_events = 0L,
      median = NA,
      hr = NA,
      lcl = NA,
      ucl = NA,
      conf_level = conf_level,
      pval = NA,
      pval_label = pval_label,
      row.names = seq_along(variables$biomarkers),
      stringsAsFactors = FALSE
    )
  }
}
