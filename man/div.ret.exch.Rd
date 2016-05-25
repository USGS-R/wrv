\name{div.ret.exch}

\alias{div.ret.exch}

\docType{data}

\title{Diversions, Returns, and Exchange Wells}

\description{
The location of streamflow diversions, irrigation canal or pond returns, and exchange well returns.
}

\usage{div.ret.exch}

\format{
An object of \code{SpatialPointsDataFrame} class containing 117 points with the following variables:
\describe{
  \item{Name}{a local name for the diversion/return site.}
  \item{Type}{the data type: \dQuote{Diversion}, \dQuote{Return}, and \dQuote{Exchange well inflow}.}
  \item{LocSource}{the data source.}
  \item{Big}{the corresponding river reach.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources, accessed on June 5, 2015
}

\examples{
plot(div.ret.exch)
str(div.ret.exch@data)
}

\keyword{datasets}
