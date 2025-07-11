% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_glm_count.R
\name{h_ppmeans}
\alias{h_ppmeans}
\title{Function to return the estimated means using predicted probabilities}
\usage{
h_ppmeans(obj, .df_row, arm, conf_level)
}
\arguments{
\item{obj}{(\code{glm.fit})\cr fitted model object used to derive the mean rate estimates in each treatment arm.}

\item{.df_row}{(\code{data.frame})\cr dataset that includes all the variables that are called in \code{.var} and \code{variables}.}

\item{arm}{(\code{string})\cr group variable, for which the covariate adjusted means of multiple groups will be
summarized. Specifically, the first level of \code{arm} variable is taken as the reference group.}

\item{conf_level}{(\code{proportion})\cr value used to derive the confidence interval for the rate.}
}
\value{
\itemize{
\item \code{h_ppmeans()} returns the estimated means.
}
}
\description{
For each arm level, the predicted mean rate is calculated using the fitted model object, with \code{newdata}
set to the result of \code{stats::model.frame}, a reconstructed data or the original data, depending on the
object formula (coming from the fit). The confidence interval is derived using the \code{conf_level} parameter.
}
\seealso{
\code{\link[=summarize_glm_count]{summarize_glm_count()}}.
}
