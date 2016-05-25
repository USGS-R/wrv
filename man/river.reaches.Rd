\name{river.reaches}

\alias{river.reaches}

\docType{data}

\title{Major River Reaches}

\description{
The major river reaches of the Wood River Valley, Idaho.
}

\usage{river.reaches}

\format{
An object of \code{SpatialLinesDataFrame} class containing 22 \code{Lines} and a \code{data.frame} with the following variables:
\describe{
  \item{Reach}{is the name of the subreaches measured in U.S. Geological Survey (USGS) seepage survey.}
  \item{BigReach}{is the name of the reaches for which time series targets are available for part or all of the calibration period.}
  \item{DrainRiver}{is the model boundary assignment, either \dQuote{drain} or \dQuote{river}.}
  \item{RchAvg}{is the estimated average reach gain in cubic meters per day for 1995-2010 based on a combination of gage data and the USGS seepage survey.}
  \item{BigRAv}{is the estimated average reach gain in cubic meters per day for 1995-2010 based on gage data.}
  \item{ReachNo}{is the reach number identifier.}
  \item{Depth}{is the estimated average depth in meters of water in reach, measured from the air-water interface to the top of the riverbed sediments.}
  \item{BedThk}{is the estimated thickness in meters of the saturated riverbed sediments.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources, accessed on June 6, 2015
}

\examples{
plot(river.reaches)
str(river.reaches@data)
}

\keyword{datasets}
