\name{swe}

\alias{swe}

\docType{data}

\title{Snow Water Equivalent}

\description{
Average daily snow water equivalent (SWE) at weather stations in the Wood River Valley and surrounding areas.
}

\usage{swe}

\format{
A \code{data.frame} object with 366 records and the following variables:
\describe{
  \item{MonthDay}{is the month and day, with a required date format of \code{MMDD}.}
  \item{Choco}{is the daily SWE recorded at the Chocolate Gulch snow telemetry (SNOTEL) weather station.}
  \item{Hailey}{is the daily SWE recorded at the Hailey Ranger Station at Hailey hydrometeorological automated data system (HADS) weather station.}
  \item{Picabo}{is the daily SWE recorded at the Picabo PICI HADS weather station.}
}
}

\source{
Idaho Department of Water Resources, accessed on April 24, 2015
}

\seealso{\code{\link{weather.stations}}, \code{\link{precip.zones}}, \code{\link{precipitation}}}

\examples{
str(swe)
}

\keyword{datasets}
