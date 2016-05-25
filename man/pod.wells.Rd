\name{pod.wells}

\alias{pod.wells}

\docType{data}

\title{Well Completions}

\description{
Well completions for pumping wells in the Wood River Valley aquifer system.
}

\usage{pod.wells}

\format{
An object of \code{SpatialPointsDataFrame} class containing 1,243 points with the following variables:
\describe{
  \item{WMISNumber}{is a unique number assigned to a water right point of diversion.}
  \item{WellUse}{is the permitted use(s) for a groundwater well.}
  \item{TopOpen1}{is the depth to the top of the first open interval in a groundwater well, in meters below land surface.}
  \item{BotOpen1}{is the depth to the bottom of the first open interval in a groundwater well, in meters below land surface.}
  \item{TopOpen2}{is the depth to the top of the second open interval in a groundwater well, in meters below land surface.}
  \item{BotOpen2}{is the depth to the bottom of the second open interval in a groundwater well, in meters below land surface.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources water rights database, accessed on November 29, 2014
}

\seealso{\code{\link{pod.gw}}}

\examples{
plot(pod.wells)
str(pod.wells@data)
}

\keyword{datasets}
