\name{alluvium.thickness}

\alias{alluvium.thickness}

\docType{data}

\title{Thickness of the Quaternary Sediment}

\description{
The estimated thickness of the Quaternary sediment in the Wood River Valley aquifer system, South-Central Idaho.
}

\usage{alluvium.thickness}

\format{
An object of \code{RasterLayer} class.
Each cell on the surface grid represents a depth measured from land surface in meters.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
The spatial grid is composed of 565 rows and 429 columns, and has cell sizes that are constant at 100 meters by 100 meters.
}

\source{
This dataset is a revised version of Plate 1 in Bartolino and Adkins (2012).
}

\references{
Bartolino, J.R., and Adkins, C.B., 2012, Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho: U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p., available at \url{http://pubs.usgs.gov/sir/2012/5053/}.
}

\examples{
col <- rainbow(255, start = 0.0, end = 0.8)
image(alluvium.thickness, col = col, asp = 1, axes = FALSE, xlab = "", ylab = "")
summary(alluvium.thickness)
}

\keyword{datasets}
