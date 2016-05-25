\name{entity.components}

\alias{entity.components}

\docType{data}

\title{Irrigation Entity Components}

\description{
Irrigation entities and their components in the Wood River Valley and surrounding areas.
An irrigation entity is defined as an area served by a group of surface-water and/or groundwater diversion(s).
}

\usage{entity.components}

\format{
A \code{list} object with components of \code{SpatialPolygonsDataFrame-class}.
There are a total of 192 components, one for each month in the 1995--2010 time period.
Linked \code{data.frame} objects have the following variables:
\describe{
  \item{EntitySrce}{a concatenation of the \code{EntityName} and \code{Source} character strings.}
  \item{mean.et}{the mean evapotranspiration (ET) on irrigated and semi-irrigated lands in meters.}
  \item{area}{the area of irrigated and semi-irrigated lands in square meters.}
  \item{PrecipZone}{the name of the precipitation zone.
    See \code{\link{precip.zones}} dataset for details.}
  \item{et.vol}{the volume of ET on irrigated and semi-irrigated lands in cubic meters.}
  \item{precip.vol}{the volume of precipitation on irrigated and semi-irrigated lands in cubic meters.}
  \item{cir.vol}{the volume of crop irrigation requirement in cubic meters (ET minus precipitation).}
  \item{EntityName}{is the name of the irrigation entity.}
  \item{Source}{is the water source: \dQuote{Mixed} for a mixture of surface water and groundwater, \dQuote{SW Only} for surface water only, and \dQuote{GW Only} for groundwater only.}
}
}

\source{
Calculated from the \code{\link{irr.entities}}, \code{\link{wetlands}}, \code{\link{public.parcels}}, \code{\link{irr.lands.year}}, \code{\link{et}}, and \code{\link{precipitation}} datasets;
see the \file{package-datasets} vignette for the \R code used in this calculation.
}

\examples{
names(entity.components)
plot(entity.components[["199506"]])
print(entity.components[["199506"]])
}

\keyword{datasets}
