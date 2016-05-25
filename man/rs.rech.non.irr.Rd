\name{rs.rech.non.irr}

\alias{rs.rech.non.irr}

\docType{data}

\title{Rasterized Monthly Recharge on Non-Irrigated Lands}

\description{
Aerial recharge and discharge on non-irrigated lands of the Wood River Valley and surrounding areas transferred to raster cells.
}

\usage{rs.rech.non.irr}

\format{
An object of \code{RasterStack} class containing 192 \code{RasterLayer} objects, one layer for each month in the 1995-2010 time period.
Each cell on a layers surface grid represents the monthly recharge in meters.
}

\source{
Calculated from the \code{et}, \code{\link{precipitation}}, \code{\link{precip.zones}}, and \code{\link{soils}} datasets;
see the \file{package-datasets} vignette for the \R code used in this calculation.
}

\examples{
names(rs.rech.non.irr)
plot(rs.rech.non.irr[["199507"]])
}

\keyword{datasets}
