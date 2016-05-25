\name{drains}

\alias{drains}

\docType{data}

\title{Drain Boundaries at Stanton Crossing and Silver Creek}

\description{
Polygons used to define the locations of drain boundaries in the model domain.
The polygons clip the line segments along the aquifer boundary (see \code{\link{alluvium.extent}}), and model cells intersecting these clipped-line segments are defined as boundary cells.
}

\usage{drains}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing a set of 2 \code{Polygons} and a \code{data.frame} with the following variable:
\describe{
  \item{Name}{is an identifier for the polygon.}
  \item{cond}{is the drain conductance in square meters per day.}
  \item{elev}{is the drain threshold elevation in meters above the North American Vertical Datum of 1988 (NAVD 88).}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
U.S. Geological Survey, accessed on March 27, 2015;
a Keyhole Markup Language (\href{http://en.wikipedia.org/wiki/Kml}{KML}) file created in \href{http://www.google.com/earth/}{Google Earth} with polygons drawn by hand in areas of known drains.
}

\examples{
str(drains)

plot(drains, border = "red")
plot(alluvium.extent, add = TRUE)
}

\keyword{datasets}
