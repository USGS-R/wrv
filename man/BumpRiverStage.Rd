\name{BumpRiverStage}

\alias{BumpRiverStage}

\title{Adjustment for Implausible River Stage}

\description{
This function decreases stage values in river cells if they are implausible with respect to water always flowing downhill.
}

\usage{
BumpRiverStage(r, outlets, min.drop = 1e-06)
}

\arguments{
\item{r}{\code{RasterLayer}; each cell on the surface grid represents a river stage.}
\item{outlets}{\code{SpatialPoints*}, \code{SpatialLines*}, \code{SpatialPolygons*} or \code{Extent}; the location of discharge outlets.
  The \code{rasterize} function is used to locate outlet cells in the raster grid \code{r}.}
\item{min.drop}{\code{numeric}; the minimum drop in stage between adjacent river cells.}
}

\details{
The \href{http://en.wikipedia.org/wiki/Lee_algorithm}{Lee algorithm} (Lee, 1961) is used to identify flow paths among the modeled river cells.
An analysis of river cell stage values along a flow path identifies any problematic cells that are obstructing downhill surface-water flow.
Stage values for these problematic cells are then lowered to an acceptable elevation.
}

\value{
Returns a \code{RasterLayer} with cell values representing the vertical change in stream stage.
These changes can be added to \code{r} to ensure that water always flows downhill.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\references{
Lee, C.Y., 1961, An algorithm for path connections and its applications: IRE Transactions on Electronic Computers, v. EC-10, no. 2, p. 346--365.
}

\examples{\dontrun{# see uncalibrated-model vignette}}

\keyword{utilities}
