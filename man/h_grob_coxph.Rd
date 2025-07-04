% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_km.R
\name{h_grob_coxph}
\alias{h_grob_coxph}
\title{Helper function to create Cox-PH grobs}
\usage{
h_grob_coxph(
  ...,
  x = 0,
  y = 0,
  width = grid::unit(0.4, "npc"),
  ttheme = gridExtra::ttheme_default(padding = grid::unit(c(1, 0.5), "lines"), core =
    list(bg_params = list(fill = c("grey95", "grey90"), alpha = 0.5)))
)
}
\arguments{
\item{...}{arguments to pass to \code{\link[=h_tbl_coxph_pairwise]{h_tbl_coxph_pairwise()}}.}

\item{x}{(\code{proportion})\cr a value between 0 and 1 specifying x-location.}

\item{y}{(\code{proportion})\cr a value between 0 and 1 specifying y-location.}

\item{width}{(\code{grid::unit})\cr width (as a unit) to use when printing the grob.}

\item{ttheme}{(\code{list})\cr see \code{\link[gridExtra:tableGrob]{gridExtra::ttheme_default()}}.}
}
\value{
A \code{grob} of a table containing statistics \code{HR}, \verb{XX\% CI} (\code{XX} taken from \code{control_coxph_pw}),
and \code{p-value (log-rank)}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

Grob of \code{rtable} output from \code{\link[=h_tbl_coxph_pairwise]{h_tbl_coxph_pairwise()}}
}
\examples{
\donttest{
library(dplyr)
library(survival)
library(grid)

grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
data <- tern_ex_adtte \%>\%
  filter(PARAMCD == "OS") \%>\%
  mutate(is_event = CNSR == 0)
tbl_grob <- h_grob_coxph(
  df = data,
  variables = list(tte = "AVAL", is_event = "is_event", arm = "ARMCD"),
  control_coxph_pw = control_coxph(conf_level = 0.9), x = 0.5, y = 0.5
)
grid::grid.draw(tbl_grob)
}

}
