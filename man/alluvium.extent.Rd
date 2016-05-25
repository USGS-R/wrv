\name{alluvium.extent}

\alias{alluvium.extent}

\docType{data}

\title{Extent of Alluvium Unit}

\description{
The estimated extent of alluvium unit in the Wood River Valley, south-central Idaho.
}

\usage{alluvium.extent}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 1 \code{Polygons}.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{Extent defined by Bartollino and Adkins (2012, Plate 1).}

\references{
Bartolino, J.R., and Adkins, C.B., 2012, Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho: U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p., available at \url{http://pubs.usgs.gov/sir/2012/5053/}.
}

\examples{
plot(alluvium.extent, col = "#BFA76F")
str(alluvium.extent)
}

\keyword{datasets}
