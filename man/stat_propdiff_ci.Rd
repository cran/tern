% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat.R
\name{stat_propdiff_ci}
\alias{stat_propdiff_ci}
\title{Proportion difference and confidence interval}
\usage{
stat_propdiff_ci(
  x,
  y,
  N_x,
  N_y,
  list_names = NULL,
  conf_level = 0.95,
  pct = TRUE
)
}
\arguments{
\item{x}{(\code{list} of \code{integer})\cr list of number of occurrences in arm X (reference group).}

\item{y}{(\code{list} of \code{integer})\cr list of number of occurrences in arm Y. Must be of equal length to \code{x}.}

\item{N_x}{(\code{numeric(1)})\cr total number of records in arm X.}

\item{N_y}{(\code{numeric(1)})\cr total number of records in arm Y.}

\item{list_names}{(\code{character})\cr names of each variable/level corresponding to pair of proportions in
\code{x} and \code{y}. Must be of equal length to \code{x} and \code{y}.}

\item{conf_level}{(\code{proportion})\cr confidence level of the interval.}

\item{pct}{(\code{flag})\cr whether output should be returned as percentages. Defaults to \code{TRUE}.}
}
\value{
List of proportion differences and CIs corresponding to each pair of number of occurrences in \code{x} and
\code{y}. Each list element consists of 3 statistics: proportion difference, CI lower bound, and CI upper bound.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Function for calculating the proportion (or risk) difference and confidence interval between arm
X (reference group) and arm Y. Risk difference is calculated by subtracting cumulative incidence
in arm Y from cumulative incidence in arm X.
}
\examples{
stat_propdiff_ci(
  x = list(0.375), y = list(0.01), N_x = 5, N_y = 5, list_names = "x", conf_level = 0.9
)

stat_propdiff_ci(
  x = list(0.5, 0.75, 1), y = list(0.25, 0.05, 0.5), N_x = 10, N_y = 20, pct = FALSE
)

}
\seealso{
Split function \code{\link[=add_riskdiff]{add_riskdiff()}} which, when used as \code{split_fun} within \code{\link[rtables:split_cols_by]{rtables::split_cols_by()}}
with \code{riskdiff} argument is set to \code{TRUE} in subsequent analyze functions, adds a column containing
proportion (risk) difference to an \code{rtables} layout.
}
