% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting_functions.R
\name{apply_auto_formatting}
\alias{apply_auto_formatting}
\title{Apply automatic formatting}
\usage{
apply_auto_formatting(.formats, x_stats, .df_row, .var)
}
\arguments{
\item{.formats}{(named \code{character} or \code{list})\cr formats for the statistics. See Details in \code{analyze_vars} for more
information on the \code{"auto"} setting.}

\item{x_stats}{(named \code{list})\cr a named list of statistics where each element corresponds
to an element in \code{.formats}, with matching names.}

\item{.df_row}{(\code{data.frame})\cr data frame across all of the columns for the given row split.}

\item{.var}{(\code{string})\cr single variable name that is passed by \code{rtables} when requested
by a statistics function.}
}
\description{
Checks if any of the listed formats in \code{.formats} are \code{"auto"}, and replaces \code{"auto"} with
the correct implementation of \code{format_auto} for the given statistics, data, and variable.
}
\keyword{internal}
