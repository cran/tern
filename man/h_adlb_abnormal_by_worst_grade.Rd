% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abnormal_by_worst_grade.R
\name{h_adlb_abnormal_by_worst_grade}
\alias{h_adlb_abnormal_by_worst_grade}
\title{Helper function to prepare \code{ADLB} for \code{\link[=count_abnormal_by_worst_grade]{count_abnormal_by_worst_grade()}}}
\usage{
h_adlb_abnormal_by_worst_grade(
  adlb,
  atoxgr = "ATOXGR",
  avisit = "AVISIT",
  worst_flag_low = "WGRLOFL",
  worst_flag_high = "WGRHIFL"
)
}
\arguments{
\item{adlb}{(\code{data.frame})\cr \code{ADLB} dataframe.}

\item{atoxgr}{(\code{character})\cr Analysis toxicity grade variable. This must be a \code{factor}
variable.}

\item{avisit}{(\code{character})\cr Analysis visit variable.}

\item{worst_flag_low}{(\code{character})\cr Worst low lab grade flag variable. This variable is
set to \code{"Y"} when indicating records of worst low lab grades.}

\item{worst_flag_high}{(\code{character})\cr Worst high lab grade flag variable. This variable is
set to \code{"Y"} when indicating records of worst high lab grades.}
}
\value{
\code{h_adlb_abnormal_by_worst_grade()} returns the \code{adlb} data frame with two new
variables: \code{GRADE_DIR} and \code{GRADE_ANL}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Helper function to prepare an \code{ADLB} data frame to be used as input in
\code{\link[=count_abnormal_by_worst_grade]{count_abnormal_by_worst_grade()}}. The following pre-processing steps are applied:
\enumerate{
\item \code{adlb} is filtered on variable \code{avisit} to only include post-baseline visits.
\item \code{adlb} is filtered on variables \code{worst_flag_low} and \code{worst_flag_high} so that only
worst grades (in either direction) are included.
\item From the standard lab grade variable \code{atoxgr}, the following two variables are derived
and added to \code{adlb}:
}
\itemize{
\item A grade direction variable (e.g. \code{GRADE_DIR}). The variable takes value \code{"HIGH"} when
\code{atoxgr > 0}, \code{"LOW"} when \code{atoxgr < 0}, and \code{"ZERO"} otherwise.
\item A toxicity grade variable (e.g. \code{GRADE_ANL}) where all negative values from \code{atoxgr} are
replaced by their absolute values.
}
\enumerate{
\item Unused factor levels are dropped from \code{adlb} via \code{\link[=droplevels]{droplevels()}}.
}
}
\examples{
h_adlb_abnormal_by_worst_grade(tern_ex_adlb) \%>\%
  dplyr::select(ATOXGR, GRADE_DIR, GRADE_ANL) \%>\%
  head(10)

}
\seealso{
\link{abnormal_by_worst_grade}
}
