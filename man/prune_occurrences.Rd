% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prune_occurrences.R
\name{prune_occurrences}
\alias{prune_occurrences}
\alias{keep_rows}
\alias{keep_content_rows}
\alias{has_count_in_cols}
\alias{has_count_in_any_col}
\alias{has_fraction_in_cols}
\alias{has_fraction_in_any_col}
\alias{has_fractions_difference}
\alias{has_counts_difference}
\title{Occurrence table pruning}
\usage{
keep_rows(row_condition)

keep_content_rows(content_row_condition)

has_count_in_cols(atleast, ...)

has_count_in_any_col(atleast, ...)

has_fraction_in_cols(atleast, ...)

has_fraction_in_any_col(atleast, ...)

has_fractions_difference(atleast, ...)

has_counts_difference(atleast, ...)
}
\arguments{
\item{row_condition}{(\code{CombinationFunction})\cr condition function which works on individual
analysis rows and flags whether these should be kept in the pruned table.}

\item{content_row_condition}{(\code{CombinationFunction})\cr condition function which works on individual
first content rows of leaf tables and flags whether these leaf tables should be kept in the pruned table.}

\item{atleast}{(\code{numeric(1)})\cr threshold which should be met in order to keep the row.}

\item{...}{arguments for row or column access, see \code{\link{rtables_access}}: either \code{col_names} (\code{character}) including
the names of the columns which should be used, or alternatively \code{col_indices} (\code{integer}) giving the indices
directly instead.}
}
\value{
\itemize{
\item \code{keep_rows()} returns a pruning function that can be used with \code{\link[rtables:prune_table]{rtables::prune_table()}}
to prune an \code{rtables} table.
}

\itemize{
\item \code{keep_content_rows()} returns a pruning function that checks the condition on the first content
row of leaf tables in the table.
}

\itemize{
\item \code{has_count_in_cols()} returns a condition function that sums the counts in the specified column.
}

\itemize{
\item \code{has_count_in_any_col()} returns a condition function that compares the counts in the
specified columns with the threshold.
}

\itemize{
\item \code{has_fraction_in_cols()} returns a condition function that sums the counts in the
specified column, and computes the fraction by dividing by the total column counts.
}

\itemize{
\item \code{has_fraction_in_any_col()} returns a condition function that looks at the fractions
in the specified columns and checks whether any of them fulfill the threshold.
}

\itemize{
\item \code{has_fractions_difference()} returns a condition function that extracts the fractions of each
specified column, and computes the difference of the minimum and maximum.
}

\itemize{
\item \code{has_counts_difference()} returns a condition function that extracts the counts of each
specified column, and computes the difference of the minimum and maximum.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Family of constructor and condition functions to flexibly prune occurrence tables.
The condition functions always return whether the row result is higher than the threshold.
Since they are of class \code{\link[=CombinationFunction]{CombinationFunction()}} they can be logically combined with other condition
functions.
}
\section{Functions}{
\itemize{
\item \code{keep_rows()}: Constructor for creating pruning functions based on
a row condition function. This removes all analysis rows (\code{TableRow}) that should be
pruned, i.e., don't fulfill the row condition. It removes the sub-tree if there are no
children left.

\item \code{keep_content_rows()}: Constructor for creating pruning functions based on
a condition for the (first) content row in leaf tables. This removes all leaf tables where
the first content row does not fulfill the condition. It does not check individual rows.
It then proceeds recursively by removing the sub tree if there are no children left.

\item \code{has_count_in_cols()}: Constructor for creating condition functions on total counts in the specified columns.

\item \code{has_count_in_any_col()}: Constructor for creating condition functions on any of the counts in
the specified columns satisfying a threshold.

\item \code{has_fraction_in_cols()}: Constructor for creating condition functions on total fraction in
the specified columns.

\item \code{has_fraction_in_any_col()}: Constructor for creating condition functions on any fraction in
the specified columns.

\item \code{has_fractions_difference()}: Constructor for creating condition function that checks the difference
between the fractions reported in each specified column.

\item \code{has_counts_difference()}: Constructor for creating condition function that checks the difference
between the counts reported in each specified column.

}}
\note{
Since most table specifications are worded positively, we name our constructor and condition
functions positively, too. However, note that the result of \code{\link[=keep_rows]{keep_rows()}} says what
should be pruned, to conform with the \code{\link[rtables:prune_table]{rtables::prune_table()}} interface.
}
\examples{
\donttest{
tab <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by("RACE") \%>\%
  split_rows_by("STRATA1") \%>\%
  summarize_row_groups() \%>\%
  analyze_vars("COUNTRY", .stats = "count_fraction") \%>\%
  build_table(DM)
}

\donttest{
# `keep_rows`
is_non_empty <- !CombinationFunction(all_zero_or_na)
prune_table(tab, keep_rows(is_non_empty))
}

# `keep_content_rows`
\donttest{
more_than_twenty <- has_count_in_cols(atleast = 20L, col_names = names(tab))
prune_table(tab, keep_content_rows(more_than_twenty))
}

\donttest{
more_than_one <- has_count_in_cols(atleast = 1L, col_names = names(tab))
prune_table(tab, keep_rows(more_than_one))
}

\donttest{
# `has_count_in_any_col`
any_more_than_one <- has_count_in_any_col(atleast = 1L, col_names = names(tab))
prune_table(tab, keep_rows(any_more_than_one))
}

\donttest{
# `has_fraction_in_cols`
more_than_five_percent <- has_fraction_in_cols(atleast = 0.05, col_names = names(tab))
prune_table(tab, keep_rows(more_than_five_percent))
}

\donttest{
# `has_fraction_in_any_col`
any_atleast_five_percent <- has_fraction_in_any_col(atleast = 0.05, col_names = names(tab))
prune_table(tab, keep_rows(any_atleast_five_percent))
}

\donttest{
# `has_fractions_difference`
more_than_five_percent_diff <- has_fractions_difference(atleast = 0.05, col_names = names(tab))
prune_table(tab, keep_rows(more_than_five_percent_diff))
}

\donttest{
more_than_one_diff <- has_counts_difference(atleast = 1L, col_names = names(tab))
prune_table(tab, keep_rows(more_than_one_diff))
}

}
