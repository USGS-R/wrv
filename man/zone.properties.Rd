\name{zone.properties}

\alias{zone.properties}

\docType{data}

\title{Hydraulic Properties of Hydrogeologic Zones}

\description{
Estimates of the hydraulic properties for each hydrogeologic zone.
}

\usage{zone.properties}

\format{
A \code{data.frame} object with the following variables:
\describe{
  \item{ID}{is a numeric identifier for the hydrogeologic zone.}
  \item{name}{is the name of the hydrogeologic zone.}
  \item{vani}{is the vertical anisotropy, a dimensionless quantity.}
  \item{sc}{is the storage coefficient, a dimensionless quantity.}
  \item{sy}{is the specific yield, a dimensionless quantity.}
  \item{hk}{is the horizontal hydraulic conductivity in meters per day.}
  \item{ss}{is the specific storage, in inverse meter.}
}
}

\source{
Bartolino, J.R., and Adkins, C.B., 2012, Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho: U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p., available at \url{http://pubs.usgs.gov/sir/2012/5053/}.
}

\examples{
str(zone.properties)
}

\keyword{datasets}
