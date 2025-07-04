% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_rtables.R
\name{c_label_n}
\alias{c_label_n}
\title{Content row function to add row total to labels}
\usage{
c_label_n(df, labelstr, .N_row)
}
\arguments{
\item{df}{(\code{data.frame})\cr data set containing all analysis variables.}

\item{labelstr}{(\code{string})\cr label of the level of the parent split currently being summarized
(must be present as second argument in Content Row Functions). See \code{\link[rtables:summarize_row_groups]{rtables::summarize_row_groups()}}
for more information.}

\item{.N_row}{(\code{integer(1)})\cr row-wise N (row group count) for the group of observations being analyzed
(i.e. with no column-based subsetting) that is typically passed by \code{rtables}.}
}
\value{
A list with formatted \code{\link[rtables:CellValue]{rtables::CellValue()}} with the row count value and the correct label.
}
\description{
This takes the label of the latest row split level and adds the row total from \code{df} in parentheses.
This function differs from \code{\link[=c_label_n_alt]{c_label_n_alt()}} by taking row counts from \code{df} rather than
\code{alt_counts_df}, and is used by \code{\link[=add_rowcounts]{add_rowcounts()}} when \code{alt_counts} is set to \code{FALSE}.
}
\note{
It is important here to not use \code{df} but rather \code{.N_row} in the implementation, because
the former is already split by columns and will refer to the first column of the data only.
}
\seealso{
\code{\link[=c_label_n_alt]{c_label_n_alt()}} which performs the same function but retrieves row counts from
\code{alt_counts_df} instead of \code{df}.
}
\keyword{internal}
