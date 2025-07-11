% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_step.R
\name{h_step}
\alias{h_step}
\alias{h_step_window}
\alias{h_step_trt_effect}
\alias{h_step_survival_formula}
\alias{h_step_survival_est}
\alias{h_step_rsp_formula}
\alias{h_step_rsp_est}
\title{Helper functions for subgroup treatment effect pattern (STEP) calculations}
\usage{
h_step_window(x, control = control_step())

h_step_trt_effect(data, model, variables, x)

h_step_survival_formula(variables, control = control_step())

h_step_survival_est(
  formula,
  data,
  variables,
  x,
  subset = rep(TRUE, nrow(data)),
  control = control_coxph()
)

h_step_rsp_formula(variables, control = c(control_step(), control_logistic()))

h_step_rsp_est(
  formula,
  data,
  variables,
  x,
  subset = rep(TRUE, nrow(data)),
  control = control_logistic()
)
}
\arguments{
\item{x}{(\code{numeric})\cr biomarker value(s) to use (without \code{NA}).}

\item{control}{(named \code{list})\cr output from \code{control_step()}.}

\item{data}{(\code{data.frame})\cr the dataset containing the variables to summarize.}

\item{model}{(\code{coxph} or \code{glm})\cr the regression model object.}

\item{variables}{(named \code{list} of \code{string})\cr list of additional analysis variables.}

\item{formula}{(\code{formula})\cr the regression model formula.}

\item{subset}{(\code{logical})\cr subset vector.}
}
\value{
\itemize{
\item \code{h_step_window()} returns a list containing the window-selection matrix \code{sel}
and the interval information matrix \code{interval}.
}

\itemize{
\item \code{h_step_trt_effect()} returns a vector with elements \code{est} and \code{se}.
}

\itemize{
\item \code{h_step_survival_formula()} returns a model formula.
}

\itemize{
\item \code{h_step_survival_est()} returns a matrix of number of observations \code{n},
\code{events}, log hazard ratio estimates \code{loghr}, standard error \code{se},
and Wald confidence interval bounds \code{ci_lower} and \code{ci_upper}. One row is
included for each biomarker value in \code{x}.
}

\itemize{
\item \code{h_step_rsp_formula()} returns a model formula.
}

\itemize{
\item \code{h_step_rsp_est()} returns a matrix of number of observations \code{n}, log odds
ratio estimates \code{logor}, standard error \code{se}, and Wald confidence interval bounds
\code{ci_lower} and \code{ci_upper}. One row is included for each biomarker value in \code{x}.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Helper functions that are used internally for the STEP calculations.
}
\section{Functions}{
\itemize{
\item \code{h_step_window()}: Creates the windows for STEP, based on the control settings
provided.

\item \code{h_step_trt_effect()}: Calculates the estimated treatment effect estimate
on the linear predictor scale and corresponding standard error from a STEP \code{model} fitted
on \code{data} given \code{variables} specification, for a single biomarker value \code{x}.
This works for both \code{coxph} and \code{glm} models, i.e. for calculating log hazard ratio or log odds
ratio estimates.

\item \code{h_step_survival_formula()}: Builds the model formula used in survival STEP calculations.

\item \code{h_step_survival_est()}: Estimates the model with \code{formula} built based on
\code{variables} in \code{data} for a given \code{subset} and \code{control} parameters for the
Cox regression.

\item \code{h_step_rsp_formula()}: Builds the model formula used in response STEP calculations.

\item \code{h_step_rsp_est()}: Estimates the model with \code{formula} built based on
\code{variables} in \code{data} for a given \code{subset} and \code{control} parameters for the
logistic regression.

}}
