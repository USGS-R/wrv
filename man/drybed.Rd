\name{drybed}

\alias{drybed}

\docType{data}

\title{Dry River Bed and Stream Fed Creek Conditions}

\description{
A summary of dry river bed and stream fed conditions in the Wood River Valley, Idaho.
Stream reaches on the Big Wood River between Glendale and Wood River Ranch are episodically dry;
these dry periods are specified for calendar months when water diversions to the Bypass Canal begins before the 16th of the month and ends after the 15th of the month.
}

\usage{drybed}

\format{
A \code{data.frame} object with 12 records and the following variables:
\describe{
  \item{Reach}{is the stream reach name.}
  \item{199501, \dots, 201012}{are logical values indicating whether the stream reach exhibits dry-bed conditions during a stress period.}
}
}

\source{
Idaho Department of Water Resources, accessed on January 6, 2016;
compiled from Water District 37 records.
}

\examples{
str(drybed)
}

\keyword{datasets}
