\name{idaho}

\alias{idaho}

\docType{data}

\title{U.S. State of Idaho}

\description{
The boundary of Idaho, a state in the northwestern region of the United States.
}

\usage{idaho}

\format{
An object of \code{SpatialPolygons} class containing 1 \code{Polygons}.
Cartographic boundary at 5m (1:5,000,000) resolution.
}

\source{
U.S. Department of Commerce, U.S. Census Bureau, Geography Division/Cartographic Products Branch.
A simplified representation of the State of Idaho from the 2014 Census Bureau's MAF/\href{http://www.census.gov/geo/maps-data/data/tiger.html}{TIGER} geographic database.
}

\examples{
plot(idaho, col = "#EAE2CF", border = "#BFA76F", lwd = 0.5)
print(idaho)
}

\keyword{datasets}
