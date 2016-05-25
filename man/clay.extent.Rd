\name{clay.extent}

\alias{clay.extent}

\docType{data}

\title{Extent of Clay Unit}

\description{
The estimated extent of the clay confining unit (aquitard) separating the unconfined aquifer from the underlying confined aquifer in the Wood River Valley.
}

\usage{clay.extent}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 2 \code{Polygons}.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Extent defined by Moreland (1977, fig. 3 in USGS Open-File report).
Moreland (1977) shows an outlier by Picabo that is assumed to indicate confined conditions in the basalt and not the lake sediments.
}

\references{
Moreland, J.A., 1977, Ground water-surface water relations in the Silver Creek area, Blaine County, Idaho: Boise, Idaho Department of Water Resources, Water Information Bulletin 44, 42 p., 5 plates in pocket, accessed January 31, 2012.
Also published as U.S. Geological Survey Open-File report 77-456, 66 p., available at \url{http://pubs.er.usgs.gov/pubs/ofr/ofr77456}.
}

\examples{
plot(clay.extent, col = "#FDC086", border = NA)
plot(alluvium.extent, add = TRUE)
str(clay.extent)
}

\keyword{datasets}
