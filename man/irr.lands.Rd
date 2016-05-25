\name{irr.lands}

\alias{irr.lands}

\docType{data}

\title{Irrigated Lands}

\description{
The irrigation classification of land area in the Wood River Valley and surrounding areas; available for years 1996, 2000, 2002, 2006, 2008, 2009, and 2010.
}

\usage{irr.lands}

\format{
A \code{list object} of length 7 with components of \code{SpatialPolygonsDataFrame-class}.
The \code{data.frame} associated with each of the \code{SpatialPolygons} objects has the following variable:
\describe{
  \item{Status}{is the status of land during the year reviewed, may be \dQuote{irrigated}, \dQuote{semi-irrigated}, or \dQuote{non-irrigated}.}
}
}

\source{
Idaho Department of Water Resources, accessed on November 17, 2014;
polygons derived from U.S. Department of Agriculture Common Land Unit polygons with some refinement of polygons.
Irrigation status interpreted using satellite imagery and aerial photography.
}

\seealso{\code{\link{irr.lands.year}}}

\examples{
spplot(irr.lands[["2010"]], "Status")
print(irr.lands)
}

\keyword{datasets}
