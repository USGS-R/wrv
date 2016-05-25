\name{RmSmallCellChunks}

\alias{RmSmallCellChunks}

\title{Remove Small Cell Chunks}

\description{
This function identifies cell chunks in a single raster grid layer,
where a cell chunk is defined as a group of connected cells with non-missing values.
The cell chunk with the largest surface area is preserved and all others removed.
}

\usage{RmSmallCellChunks(r)}

\arguments{
\item{r}{\code{RasterLayer}; a raster grid layer with cell values.}
}

\value{
The raster grid layer \code{r} with cell values in the smaller cell chunks set to \code{NA}.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{clump}}

\examples{
set.seed(0)
r <- raster(ncols = 10, nrows = 10)
r[] <- round(runif(ncell(r)) * 0.7)
r <- clump(r)
plot(r)

r.new <- RmSmallCellChunks(r)
plot(r.new, zlim = range(r[], na.rm = TRUE))
}

\keyword{utilities}
