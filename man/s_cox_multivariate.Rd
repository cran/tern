% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coxph.R
\name{s_cox_multivariate}
\alias{s_cox_multivariate}
\title{Multivariate Cox model - summarized results}
\usage{
s_cox_multivariate(
  formula,
  data,
  conf_level = 0.95,
  pval_method = c("wald", "likelihood"),
  ...
)
}
\arguments{
\item{formula}{(\code{formula})\cr a formula corresponding to the investigated \code{\link[survival:Surv]{survival::Surv()}} survival model
including covariates.}

\item{data}{(\code{data.frame})\cr a data frame which includes the variable in formula and covariates.}

\item{conf_level}{(\code{proportion})\cr the confidence level for the hazard ratio interval estimations. Default is 0.95.}

\item{pval_method}{(\code{string})\cr the method used for the estimation of p-values, should be one of
\code{"wald"} (default) or \code{"likelihood"}.}

\item{...}{optional parameters passed to \code{\link[survival:coxph]{survival::coxph()}}. Can include \code{ties}, a character string specifying the
method for tie handling, one of \code{exact} (default), \code{efron}, \code{breslow}.}
}
\value{
A \code{list} with elements \code{mod}, \code{msum}, \code{aov}, and \code{coef_inter}.
}
\description{
Analyses based on multivariate Cox model are usually not performed for the Controlled Substance Reporting or
regulatory documents but serve exploratory purposes only (e.g., for publication). In practice, the model usually
includes only the main effects (without interaction terms). It produces the hazard ratio estimates for each of the
covariates included in the model.
The analysis follows the same principles (e.g., stratified vs. unstratified analysis and tie handling) as the
usual Cox model analysis. Since there is usually no pre-specified hypothesis testing for such analysis,
the p.values need to be interpreted with caution. (\strong{Statistical Analysis of Clinical Trials Data with R},
\verb{NEST's bookdown})
}
\details{
The output is limited to single effect terms. Work in ongoing for estimation of interaction terms
but is out of scope as defined by the  Global Data Standards Repository
(\strong{\code{GDS_Standard_TLG_Specs_Tables_2.doc}}).
}
\examples{
library(dplyr)

adtte <- tern_ex_adtte
adtte_f <- subset(adtte, PARAMCD == "OS") # _f: filtered
adtte_f <- filter(
  adtte_f,
  PARAMCD == "OS" &
    SEX \%in\% c("F", "M") &
    RACE \%in\% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
)
adtte_f$SEX <- droplevels(adtte_f$SEX)
adtte_f$RACE <- droplevels(adtte_f$RACE)

}
\seealso{
\code{\link[=estimate_coef]{estimate_coef()}}.
}
\keyword{internal}
