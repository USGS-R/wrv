\name{obs.wells.head}

\alias{obs.wells.head}

\docType{data}

\title{Hydraulic Heads in Observation Wells}

\description{
Hydraulic-head (groundwater-level) measurements recorded in observation wells in the Wood River Valley aquifer system.
Values are used as observations during the parameter estimation process.
}

\usage{obs.wells.head}

\format{
A \code{data.frame} object with 3,477 records and the following variables:
\describe{
  \item{PESTNAME}{is a unique well identifier for PEST.}
  \item{DateTime}{is the date and time when the measurement was recorded.}
  \item{Head}{is the groundwater-level measurement (hydraulic head) in meters above NAVD 88.}
}
}

\source{
Idaho Department of Water Resources, accessed on June 26, 2015
}

\seealso{\code{\link{obs.wells}}}

\examples{
str(obs.wells.head)
}

\keyword{datasets}
