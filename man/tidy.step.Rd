% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_step.R
\name{tidy.step}
\alias{tidy.step}
\title{Custom tidy method for STEP results}
\usage{
\method{tidy}{step}(x, ...)
}
\arguments{
\item{x}{(\code{matrix})\cr results from \code{\link[=fit_survival_step]{fit_survival_step()}}.}

\item{...}{not used.}
}
\value{
A \code{tibble} with one row per STEP subgroup. The estimates and CIs are on the HR or OR scale,
respectively. Additional attributes carry metadata also used for plotting.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Tidy the STEP results into a \code{tibble} format ready for plotting.
}
\examples{
library(survival)
lung$sex <- factor(lung$sex)
vars <- list(
  time = "time",
  event = "status",
  arm = "sex",
  biomarker = "age"
)
step_matrix <- fit_survival_step(
  variables = vars,
  data = lung,
  control = c(control_coxph(), control_step(num_points = 10, degree = 2))
)
broom::tidy(step_matrix)

}
\seealso{
\code{\link[=g_step]{g_step()}} which consumes the result from this function.
}
