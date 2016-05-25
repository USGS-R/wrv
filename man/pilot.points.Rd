\name{pilot.points}

\alias{pilot.points}

\docType{data}

\title{Pilot Points}

\description{
Location of pilot points in the model domain.
}

\usage{pilot.points}

\format{
An object of \code{SpatialPointsDataFrame} class containing 106 points with the following variables:
\describe{
  \item{Layer}{is the model layer.}
  \item{Zone}{is the kriging zone.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources, accessed on June 11, 2015
}

\seealso{\code{\link{kriging.zones}}}

\examples{
plot(pilot.points)
str(pilot.points@data)
}

\keyword{datasets}
