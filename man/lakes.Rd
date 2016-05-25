\name{lakes}

\alias{lakes}

\docType{data}

\title{Lakes and Reservoirs}

\description{
Lakes and reservoirs of the Wood River Valley and surrounding areas.
}

\usage{lakes}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 55 \code{Polygons}.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}), accessed on April 2, 2014
}

\examples{
plot(lakes, col = "#CCFFFF", border = "#3399CC", lwd = 0.5)
str(lakes@data)
}

\keyword{datasets}
