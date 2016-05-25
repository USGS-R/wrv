\name{subreach.recharge}

\alias{subreach.recharge}

\docType{data}

\title{Stream-Aquifer Flow Exchange Along River Subreaches}

\description{
Stream-aquifer flow exchange along river subreaches specified as aquifer recharge.
Values used as observations in parameter estimation.
}

\usage{subreach.recharge}

\format{
A \code{data.frame} object with 19 records and the following variables:
\describe{
  \item{ReachNo}{is the subreach number identifier.}
  \item{Reach}{is the name of the subreach.}
  \item{BigReachNo}{is the reach number identifier.}
  \item{BigReach}{is the name of the reach.}
  \item{Aug}{the estimated volumetric flow rate measured during August 2012, in cubic meters per day.}
  \item{Oct}{the estimated volumetric flow rate measured during October 2012, in cubic meters per day.}
  \item{Mar}{the estimated volumetric flow rate measured during March 2013, in cubic meters per day.}
}
}

\details{
A positive stream-aquifer flow exchange indicates aquifer recharge (also know as a losing river subreach).
}

\source{
Flow values calculated from \code{seepage.study} data.
}

\examples{
str(subreach.recharge)
}

\keyword{datasets}
