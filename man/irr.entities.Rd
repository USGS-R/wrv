\name{irr.entities}

\alias{irr.entities}

\docType{data}

\title{Irrigation Entities}

\description{
The delineation of areas served by a group of surface-water and (or) groundwater diversions.
}

\usage{irr.entities}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 235 \code{Polygons} and a \code{data.frame} with the following variables:
\describe{
  \item{EntityName}{is the name of the irrigation entity served by a group of diversions.}
  \item{Source}{is the water source: \dQuote{Mixed} for a mixture of surface water and groundwater, \dQuote{SW Only} for surface-water only, and \dQuote{GW Only} for groundwater only.}
  \item{EntitySrce}{is a concatenation of the \code{EntityName} and \code{Source} character strings.}
  \item{PrecipZone}{is the name of the precipitation zone.
    See \code{\link{precip.zones}} dataset for details.}
}
}

\source{
Idaho Department of Water Resources (IDWR), accessed on December 11, 2014;
derived from IDWR water rights database, Blaine County tax lot data, and IDWR irrigated land classification files.
}

\examples{
plot(irr.entities)
print(irr.entities)
}

\keyword{datasets}
