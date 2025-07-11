% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_forest.R
\name{forest_viewport}
\alias{forest_viewport}
\title{Create a viewport tree for the forest plot}
\usage{
forest_viewport(
  tbl,
  width_row_names = NULL,
  width_columns = NULL,
  width_forest = grid::unit(1, "null"),
  gap_column = grid::unit(1, "lines"),
  gap_header = grid::unit(1, "lines"),
  mat_form = NULL
)
}
\arguments{
\item{tbl}{(\code{VTableTree})\cr \code{rtables} table object.}

\item{width_row_names}{(\code{grid::unit})\cr width of row names.}

\item{width_columns}{(\code{grid::unit})\cr width of column spans.}

\item{width_forest}{(\code{grid::unit})\cr width of the forest plot.}

\item{gap_column}{(\code{grid::unit})\cr gap width between the columns.}

\item{gap_header}{(\code{grid::unit})\cr gap width between the header.}

\item{mat_form}{(\code{MatrixPrintForm})\cr matrix print form of the table.}
}
\value{
A viewport tree.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\examples{
library(grid)

tbl <- rtable(
  header = rheader(
    rrow("", "E", rcell("CI", colspan = 2)),
    rrow("", "A", "B", "C")
  ),
  rrow("row 1", 1, 0.8, 1.1),
  rrow("row 2", 1.4, 0.8, 1.6),
  rrow("row 3", 1.2, 0.8, 1.2)
)

\donttest{
v <- forest_viewport(tbl)

grid::grid.newpage()
showViewport(v)
}

}
