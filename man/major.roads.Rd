\name{major.roads}

\alias{major.roads}

\docType{data}

\title{Major Roads}

\description{
Major roads in the Wood River Valley and surrounding areas.
}

\usage{major.roads}

\format{
An object of \code{SpatialLinesDataFrame} class containing 475 \code{Lines}.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}), accessed on October 20, 2015
}

\examples{
plot(major.roads)
str(major.roads@data)
}

\keyword{datasets}
