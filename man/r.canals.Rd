\name{r.canals}

\alias{r.canals}

\docType{data}

\title{Rasterized Canals}

\description{
Canal systems of the Wood River Valley and surrounding areas transferred to raster cells.
}

\usage{r.canals}

\format{
An object of \code{RasterLayer} class with indexed cell values linked to a raster attribute table (RAT).
The RAT is a \code{data.frame} with the following components:
\describe{
  \item{ID}{the integer cell index.}
  \item{COUNT}{the frequency of the cell index in the raster grid.}
  \item{EntityName}{the name of the irrigation entity served by the canal system.}
}
}

\source{
Calculated by transferring the \code{\link{canals}} dataset to grid cells in the \code{\link{land.surface}} dataset using the \code{rasterize} function;
see the \file{package-datasets} vignette for the \R code used in this calculation.
}

\examples{
plot(r.canals)
print(levels(r.canals)[[1]])
}

\keyword{datasets}
