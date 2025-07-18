% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_km.R
\name{h_tbl_coxph_pairwise}
\alias{h_tbl_coxph_pairwise}
\title{Helper function for generating a pairwise Cox-PH table}
\usage{
h_tbl_coxph_pairwise(
  df,
  variables,
  ref_group_coxph = NULL,
  control_coxph_pw = control_coxph(),
  annot_coxph_ref_lbls = FALSE
)
}
\arguments{
\item{df}{(\code{data.frame})\cr data set containing all analysis variables.}

\item{variables}{(named \code{list})\cr variable names. Details are:
\itemize{
\item \code{tte} (\code{numeric})\cr variable indicating time-to-event duration values.
\item \code{is_event} (\code{logical})\cr event variable. \code{TRUE} if event, \code{FALSE} if time to event is censored.
\item \code{arm} (\code{factor})\cr the treatment group variable.
\item \code{strata} (\code{character} or \code{NULL})\cr variable names indicating stratification factors.
}}

\item{ref_group_coxph}{(\code{string} or \code{NULL})\cr level of arm variable to use as reference group in calculations for
\code{annot_coxph} table. If \code{NULL} (default), uses the first level of the arm variable.}

\item{control_coxph_pw}{(\code{list})\cr parameters for comparison details, specified using the helper function
\code{\link[=control_coxph]{control_coxph()}}. Some possible parameter options are:
\itemize{
\item \code{pval_method} (\code{string})\cr p-value method for testing hazard ratio = 1.
Default method is \code{"log-rank"}, can also be set to \code{"wald"} or \code{"likelihood"}.
\item \code{ties} (\code{string})\cr method for tie handling. Default is \code{"efron"},
can also be set to \code{"breslow"} or \code{"exact"}. See more in \code{\link[survival:coxph]{survival::coxph()}}
\item \code{conf_level} (\code{proportion})\cr confidence level of the interval for HR.
}}

\item{annot_coxph_ref_lbls}{(\code{flag})\cr whether the reference group should be explicitly printed in labels for the
\code{annot_coxph} table. If \code{FALSE} (default), only comparison groups will be printed in \code{annot_coxph} table labels.}
}
\value{
A \code{data.frame} containing statistics \code{HR}, \verb{XX\% CI} (\code{XX} taken from \code{control_coxph_pw}),
and \code{p-value (log-rank)}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Create a \code{data.frame} of pairwise stratified or unstratified Cox-PH analysis results.
}
\examples{
library(dplyr)

adtte <- tern_ex_adtte \%>\%
  filter(PARAMCD == "OS") \%>\%
  mutate(is_event = CNSR == 0)

h_tbl_coxph_pairwise(
  df = adtte,
  variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
  control_coxph_pw = control_coxph(conf_level = 0.9)
)

}
