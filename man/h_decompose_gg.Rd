% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_km.R
\name{h_decompose_gg}
\alias{h_decompose_gg}
\title{\code{ggplot} decomposition}
\usage{
h_decompose_gg(gg)
}
\arguments{
\item{gg}{(\code{ggplot})\cr a graphic to decompose.}
}
\value{
A named \code{list} with elements:
\itemize{
\item \code{panel}: The panel.
\item \code{yaxis}: The y-axis.
\item \code{xaxis}: The x-axis.
\item \code{xlab}: The x-axis label.
\item \code{ylab}: The y-axis label.
\item \code{guide}: The legend.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

The elements composing the \code{ggplot} are extracted and organized in a \code{list}.
}
\examples{
\donttest{
library(dplyr)
library(survival)
library(grid)

fit_km <- tern_ex_adtte \%>\%
  filter(PARAMCD == "OS") \%>\%
  survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
data_plot <- h_data_plot(fit_km = fit_km)
xticks <- h_xticks(data = data_plot)
gg <- h_ggkm(
  data = data_plot,
  yval = "Survival",
  censor_show = TRUE,
  xticks = xticks, xlab = "Days", ylab = "Survival Probability",
  title = "tt",
  footnotes = "ff"
)

g_el <- h_decompose_gg(gg)
grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "red", fill = "gray85", lwd = 5))
grid::grid.draw(g_el$panel)

grid::grid.newpage()
grid.rect(gp = grid::gpar(lty = 1, col = "royalblue", fill = "gray85", lwd = 5))
grid::grid.draw(with(g_el, cbind(ylab, yaxis)))
}

}
