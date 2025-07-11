% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting_functions.R
\name{format_count_fraction_fixed_dp}
\alias{format_count_fraction_fixed_dp}
\title{Format count and percentage with fixed single decimal place}
\usage{
format_count_fraction_fixed_dp(x, ...)
}
\arguments{
\item{x}{(\code{numeric(2)})\cr vector of length 2 with count and fraction, respectively.}

\item{...}{not used. Required for \code{rtables} interface.}
}
\value{
A string in the format \verb{count (fraction \%)}. If \code{count} is 0, the format is \code{0}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Formats a count together with fraction with special consideration when count is \code{0}.
}
\examples{
format_count_fraction_fixed_dp(x = c(2, 0.6667))
format_count_fraction_fixed_dp(x = c(2, 0.5))
format_count_fraction_fixed_dp(x = c(0, 0))

}
\seealso{
Other formatting functions: 
\code{\link{extreme_format}},
\code{\link{format_auto}()},
\code{\link{format_count_fraction}()},
\code{\link{format_count_fraction_lt10}()},
\code{\link{format_extreme_values}()},
\code{\link{format_extreme_values_ci}()},
\code{\link{format_fraction}()},
\code{\link{format_fraction_fixed_dp}()},
\code{\link{format_fraction_threshold}()},
\code{\link{format_sigfig}()},
\code{\link{format_xx}()},
\code{\link{formatting_functions}}
}
\concept{formatting functions}
