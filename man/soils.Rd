\name{soils}

\alias{soils}

\docType{data}

\title{Soil Units}

\description{
Representation of mapped surficial soil units created by the Idaho Office of the National Resource Conservation Service (NRCS).
Soils have been assigned a percolation rate based on the average, saturated hydraulic conductivity of the soils as classified using the Unified Soil Classification System (USCS).
}

\usage{soils}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 718 \code{Polygons} and a \code{data.frame} with the following variables:
\describe{
  \item{GroupSymbol}{is a soil class identifier.}
  \item{SoilLayer}{is an identifier used to differentiate the soil data source used to create the soils map.
    Data sources are either \sQuote{USCS} or \sQuote{STATSGO}, the NRCS State Soil Geographic Data Base.}
  \item{SoilClass}{is a description of the soil class.}
  \item{MinRate}{is the lower percolation rate limit for the soil class, in meters per month.}
  \item{MaxRate}{is the upper percolation rate limit for the soil class, in meters per month.}
  \item{PercolationRate}{is the percolation rate in meters per month.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources, accessed on April 22, 2015
}

\examples{
spplot(soils, "PercolationRate")
str(soils@data)
}

\keyword{datasets}
