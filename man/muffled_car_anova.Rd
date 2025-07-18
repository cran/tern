% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cox_regression.R
\name{muffled_car_anova}
\alias{muffled_car_anova}
\title{Muffled \code{car::Anova}}
\usage{
muffled_car_anova(mod, test_statistic)
}
\arguments{
\item{mod}{(\code{coxph})\cr Cox regression model fitted by \code{\link[survival:coxph]{survival::coxph()}}.}

\item{test_statistic}{(\code{string})\cr the method used for estimation of p.values; \code{wald} (default) or \code{likelihood}.}
}
\value{
The output of \code{\link[car:Anova]{car::Anova()}}, with convergence message muffled.
}
\description{
Applied on survival models, \code{\link[car:Anova]{car::Anova()}} signal that the \code{strata} terms is dropped from the model formula when
present, this function deliberately muffles this message.
}
\keyword{internal}
