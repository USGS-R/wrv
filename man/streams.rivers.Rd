\name{streams.rivers}

\alias{streams.rivers}

\docType{data}

\title{Streams and Rivers}

\description{
Streams and rivers of the Wood River Valley and surrounding areas.
}

\usage{streams.rivers}

\format{
An object of \code{SpatialLinesDataFrame} class containing 581 \code{Lines}.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}), accessed on April 2, 2014
}

\examples{
plot(streams.rivers, col = "#3399CC")
str(streams.rivers@data)
}

\keyword{datasets}
