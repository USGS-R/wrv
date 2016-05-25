\name{SetPolygons}

\alias{SetPolygons}

\title{Analysis of Multi-Polygon Objects}

\description{
Determines the intersection or difference between two multi-polygon objects.
}

\usage{
SetPolygons(x, y, cmd = c("gIntersection", "gDifference"), buffer.width = NA)
}

\arguments{
\item{x}{\code{SpatialPolygons*}; a multi-polygon object.}
\item{y}{\code{SpatialPolygons*} or \code{Extent}; a multi-polygon object.}
\item{cmd}{\code{character}; specifying \code{"gIntersection"}, the default, cuts out portions of the \code{x} polygons that overlay the \code{y} polygons.
  If \code{"gDifference"} is specified, only those portions of the \code{x} polygons falling outside the \code{y} polygons are copied to the output polygons.}
\item{buffer.width}{\code{numeric}; expands or contracts the geometry of \code{y} to include the area within the specified width, see \code{gBuffer}.
  Specifying \code{NA}, the default, indicates no buffer.}
}

\details{
This function tests if the resulting geometry is valid, see \code{gIsValid}.
}

\value{
Returns an object of \code{SpatialPolygons*} class.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{gIntersection}, \code{gDifference}}

\examples{
library(sp)

m1a <- matrix(c(17.5, 24.7, 22.6, 16.5, 55.1, 55.0, 61.1, 59.7), nrow = 4, ncol = 2)
m1b <- m1a
m1b[, 1] <- m1b[, 1] + 11
p1 <- SpatialPolygons(list(Polygons(list(Polygon(m1a, FALSE), Polygon(m1b, FALSE)), 1)))
plot(p1, col = "blue")

m2a <- matrix(c(19.6, 35.7, 28.2, 60.0, 58.8, 64.4), nrow = 3, ncol = 2)
m2b <- matrix(c(20.6, 30.9, 27.3, 56.2, 53.8, 51.4), nrow = 3, ncol = 2)
p2 <- SpatialPolygons(list(Polygons(list(Polygon(m2a, FALSE), Polygon(m2b, FALSE)), 2)))
plot(p2, col = "red", add = TRUE)

p <- SetPolygons(p1, p2, "gIntersection")
plot(p, col = "green", add = TRUE)

p <- SetPolygons(p2, p1, "gDifference")
plot(p, col = "purple", add = TRUE)
}

\keyword{utilities}
