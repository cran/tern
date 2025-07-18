% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desctools_binom_diff.R
\name{desctools_binom}
\alias{desctools_binom}
\alias{desctools_binomci}
\title{Confidence intervals for a difference of binomials}
\usage{
desctools_binom(
  x1,
  n1,
  x2,
  n2,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  method = c("ac", "wald", "waldcc", "score", "scorecc", "mn", "mee", "blj", "ha", "hal",
    "jp")
)

desctools_binomci(
  x,
  n,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys", "modified wilson",
    "wilsoncc", "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting",
    "pratt", "midp", "lik", "blaker"),
  rand = 123,
  tol = 1e-05
)
}
\arguments{
\item{conf.level}{(\code{proportion})\cr confidence level, defaults to 0.95.}

\item{sides}{(\code{string})\cr side of the confidence interval to compute. Must be one of \code{"two-sided"} (default),
\code{"left"}, or \code{"right"}.}

\item{method}{(\code{string})\cr method to use. Can be one out of: \code{"wald"}, \code{"wilson"}, \code{"wilsoncc"},
\code{"agresti-coull"}, \code{"jeffreys"}, \code{"modified wilson"}, \code{"modified jeffreys"}, \code{"clopper-pearson"}, \code{"arcsine"},
\code{"logit"}, \code{"witting"}, \code{"pratt"}, \code{"midp"}, \code{"lik"}, and \code{"blaker"}.}

\item{x}{(\code{integer(1)})\cr number of successes.}

\item{n}{(\code{integer(1)})\cr number of trials.}
}
\value{
A \code{matrix} of 3 values:
\itemize{
\item \code{est}: estimate of proportion difference.
\item \code{lwr.ci}: estimate of lower end of the confidence interval.
\item \code{upr.ci}: estimate of upper end of the confidence interval.
}

A \code{matrix} with 3 columns containing:
\itemize{
\item \code{est}: estimate of proportion difference.
\item \code{lwr.ci}: lower end of the confidence interval.
\item \code{upr.ci}: upper end of the confidence interval.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Several confidence intervals for the difference between proportions.
}
\section{Functions}{
\itemize{
\item \code{desctools_binom()}: Several confidence intervals for the difference between proportions.

\item \code{desctools_binomci()}: Compute confidence intervals for binomial proportions.

}}
\keyword{internal}
