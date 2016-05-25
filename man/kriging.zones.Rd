\name{kriging.zones}

\alias{kriging.zones}

\docType{data}

\title{Kriging Zones}

\description{
The location of kriging zones in the Wood River Valley, used in parameter estimation.
}

\usage{kriging.zones}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 18 \code{Polygons} and a \code{data.frame} with the following variables:
\describe{
  \item{Zone}{is the kriging zone, interpolation in this zone is based on the parameter values of pilot points located within this zone.}
  \item{Layer}{is the model layer.}
  \item{Name}{is the local name.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources
}

\seealso{\code{\link{pilot.points}}}

\examples{
str(kriging.zones@data)
}

\keyword{datasets}
