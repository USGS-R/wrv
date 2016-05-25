\name{BumpDisconnectCells}

\alias{BumpDisconnectCells}

\title{Adjustment for Vertically Disconnected Cells}

\description{
This function decreases model cell values (such as, land-surface elevations) in the lower raster layer if they violate a minimum vertical overlap between adjacent cells.
}

\usage{
BumpDisconnectCells(rs, min.overlap = 2, bump.by = 0.1, max.itr = 1e+04)
}

\arguments{
\item{rs}{\code{RasterStack}; a collection of two raster layers, the first and second layers represent the top and bottom of a model layer.}
\item{min.overlap}{\code{numeric}; the minimum vertical overlap between adjacent cells.}
\item{bump.by}{\code{numeric}; the amount to decrease a cell value by during each iteration of the algorithm.}
\item{max.itr}{\code{numeric}; the maximum number of iterations.}
}

\details{
During each iteration of the algorithm:
(1) Cells are identified that violate the minimum vertical overlap between adjacent cells; that is, the bottom of cell \code{i} is greater than or equal to the top of an adjacent cell \code{j} minus the minimum overlap specified by the \code{min.overlap} argument.
(2) For cells violating the minimum vertical overlap, lower raster layer (\code{rs[[2]]}) values are decreased by the value specified in the \code{bump.by} argument.
}

\value{
Returns a \code{RasterLayer} that can be added to \code{rs[[2]]} to ensure connectivity between cells.
Cell values in the returned raster grid represent vertical adjustments.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\examples{
set.seed(0)
r.top <- raster(ncols = 10, nrows = 10)
r.bot <- raster(ncols = 10, nrows = 10)
r.top[] <- rnorm(ncell(r.top), mean = 12)
r.bot[] <- rnorm(ncell(r.bot), mean = 10)
summary(r.top - r.bot)

r <- BumpDisconnectCells(stack(r.top, r.bot), min.overlap = 0.1)
plot(r.bot + r)
}

\keyword{utilities}
