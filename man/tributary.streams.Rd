\name{tributary.streams}

\alias{tributary.streams}

\docType{data}

\title{Streams and Rivers}

\description{
Tributary streams of the upper Wood River Valley and surrounding areas.
}

\usage{tributary.streams}

\format{
An object of \code{SpatialLinesDataFrame} class containing 88 \code{Lines}.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources, accessed on June 1, 2015
}

\examples{
plot(tributary.streams, col = "#3399CC")
str(tributary.streams@data)
}

\keyword{datasets}
