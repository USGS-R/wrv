\name{cities}

\alias{cities}

\docType{data}

\title{Cities and Towns}

\description{Cities and towns in the Wood River Valley and surrounding areas.}

\usage{cities}

\format{
An object of \code{SpatialPointsDataFrame} class containing 11 points.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}), accessed on April 15, 2015
}

\examples{
str(cities)

col <- "#333333"
plot(cities, pch = 15, cex = 0.8, col = col)
text(cities, labels = cities@data$FEATURE_NA, col = col, cex = 0.5, pos = 1, offset = 0.4)
}

\keyword{datasets}
