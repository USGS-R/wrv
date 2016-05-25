\name{misc.locations}

\alias{misc.locations}

\docType{data}

\title{Miscellaneous Locations}

\description{
Miscellaneous locations in the Bellevue triangle area.
}

\usage{misc.locations}

\format{
An object of \code{SpatialPointsDataFrame} class containing 3 points with the following variable:
\describe{
  \item{label}{is a description of the point location.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}), accessed on December 23, 2015
}

\examples{
plot(misc.locations, pch = 20, col = "red")
text(misc.locations, labels = misc.locations@data$label, pos = 3, cex = 0.6)
}

\keyword{datasets}
