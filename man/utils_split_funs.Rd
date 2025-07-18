% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_split_funs.R
\name{utils_split_funs}
\alias{utils_split_funs}
\alias{ref_group_position}
\alias{level_order}
\title{Custom split functions}
\usage{
ref_group_position(position = "first")

level_order(order)
}
\arguments{
\item{position}{(\code{string} or \code{integer})\cr position to use for the reference group facet. Can be \code{"first"},
\code{"last"}, or a specific position.}

\item{order}{(\code{character} or \code{numeric})\cr vector of ordering indices for the split facets.}
}
\value{
\itemize{
\item \code{ref_group_position()} returns an utility function that puts the reference group
as first, last or at a certain position and needs to be assigned to \code{split_fun}.
}

\itemize{
\item \code{level_order()} returns an utility function that changes the original levels' order,
depending on input \code{order} and split levels.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Collection of useful functions that are expanding on the core list of functions
provided by \code{rtables}. See \link[rtables:custom_split_funs]{rtables::custom_split_funs} and \code{\link[rtables:make_split_fun]{rtables::make_split_fun()}}
for more information on how to make a custom split function. All these functions
work with \code{\link[rtables:split_rows_by]{rtables::split_rows_by()}} argument \code{split_fun} to modify the way the split
happens. For other split functions, consider consulting \code{\link[rtables:split_funcs]{rtables::split_funcs}}.
}
\section{Functions}{
\itemize{
\item \code{ref_group_position()}: Split function to place reference group facet at a specific position
during post-processing stage.

\item \code{level_order()}: Split function to change level order based on an \code{integer}
vector or a \code{character} vector that represent the split variable's factor levels.

}}
\examples{
library(dplyr)

dat <- data.frame(
  x = factor(letters[1:5], levels = letters[5:1]),
  y = 1:5
)

# With rtables layout functions
basic_table() \%>\%
  split_cols_by("x", ref_group = "c", split_fun = ref_group_position("last")) \%>\%
  analyze("y") \%>\%
  build_table(dat)

# With tern layout funcitons
adtte_f <- tern_ex_adtte \%>\%
  filter(PARAMCD == "OS") \%>\%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0
  )

basic_table() \%>\%
  split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position("first")) \%>\%
  add_colcounts() \%>\%
  surv_time(
    vars = "AVAL",
    var_labels = "Survival Time (Months)",
    is_event = "is_event",
  ) \%>\%
  build_table(df = adtte_f)

basic_table() \%>\%
  split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position(2)) \%>\%
  add_colcounts() \%>\%
  surv_time(
    vars = "AVAL",
    var_labels = "Survival Time (Months)",
    is_event = "is_event",
  ) \%>\%
  build_table(df = adtte_f)

# level_order --------
# Even if default would bring ref_group first, the original order puts it last
basic_table() \%>\%
  split_cols_by("Species", split_fun = level_order(c(1, 3, 2))) \%>\%
  analyze("Sepal.Length") \%>\%
  build_table(iris)

# character vector
new_order <- level_order(levels(iris$Species)[c(1, 3, 2)])
basic_table() \%>\%
  split_cols_by("Species", ref_group = "virginica", split_fun = new_order) \%>\%
  analyze("Sepal.Length") \%>\%
  build_table(iris)

}
\seealso{
\code{\link[rtables:make_split_fun]{rtables::make_split_fun()}}
}
