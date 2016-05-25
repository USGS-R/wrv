\name{rs.entities}

\alias{rs.entities}

\docType{data}

\title{Rasterized Monthly Irrigation Entities}

\description{
Irrigation entities of the Wood River Valley and surrounding areas transferred to raster cells.
}

\usage{rs.entities}

\format{
An object of class \code{RasterStack} class containing a 192 \code{RasterLayer} objects, one layer for each month in the 1995-2010 time period.
For each raster layer, indexed cell values are linked to a raster attribute table (RAT).
The RAT is a \code{data.frame} with the following components:
\describe{
  \item{ID}{the integer cell index.}
  \item{COUNT}{the frequency of the cell index in the raster grid.}
  \item{EntityName}{the name of the irrigation entity served by a group of diversions.}
}
}

\source{
Calculated by transferring the \code{\link{entity.components}} dataset to grid cells in the \code{\link{land.surface}} dataset using the \code{rasterize} function;
see the \file{package-datasets} vignette for the \R code used in this calculation.
}

\examples{
names(rs.entities)
plot(rs.entities[["199507"]])
print(levels(rs.entities[["199507"]])[[1]])
}

\keyword{datasets}
