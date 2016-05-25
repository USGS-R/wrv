\name{streamgages}

\alias{streamgages}

\docType{data}

\title{Streamgages}

\description{
Select streamgages in the Wood River Valley.
}

\usage{streamgages}

\format{
An object of \code{SpatialPointsDataFrame} class containing 9 points and a \code{data.frame} with the following variable:
\describe{
  \item{SiteNo}{the unique site number for the streamgage.}
  \item{SiteName}{the official name of the streamgage.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
National Water Information System (\href{http://waterdata.usgs.gov/nwis}{NWIS}), accessed on May 29, 2015.
}

\examples{
str(streamgages)
}

\keyword{datasets}
