\name{obs.wells}

\alias{obs.wells}

\docType{data}

\title{Observation Wells}

\description{
Observation wells in the Wood River Valley aquifer system.
}

\usage{obs.wells}

\format{
An object of \code{SpatialPointsDataFrame} class containing 776 points with the following variables:
\describe{
  \item{id}{a unique well identifier used in this study.}
  \item{SiteNo}{a unique well identifier within the National Water Information System (NWIS).}
  \item{SITEIDIDWR}{a unique well identifier within the Idaho Department of Water Resources (IDWR) hydrologic database.}
  \item{WELLNUMBER}{the USGS or IDWR site name for the well.}
  \item{PESTNAME}{a unique well identifier for PEST.}
  \item{METHODDRIL}{the drilling method.}
  \item{TOTALDEPTH}{the depth at which drilling stopped, in feet.}
  \item{OPENINGMIN}{the top of the screened interval, in feet.}
  \item{OPENINGMAX}{the bottom of the screened interval, in feet.}
  \item{COMPLETION}{the date on which the well drilling and construction stopped.}
  \item{WCWELLID}{the well construction well identifier.}
  \item{ALTITUDE}{the land surface elevation, in feet.}
  \item{ALTMETHOD}{the method for obtaining the land surface elevation.}
  \item{XYMETHOD}{the method of obtaining the spatial coordinates.}
  \item{BASINNO}{the basin number.}
  \item{COUNTYNAME}{the Idaho county name.}
  \item{TWPRGE}{the township and range the well is located in.}
  \item{SITENAME}{a local name for the well.}
  \item{desc}{is a description of the well type.}
  \item{TopOpen1}{is the depth to the top of the first open interval in a groundwater well, in meters below land surface.}
  \item{BotOpen1}{is the depth to the bottom of the first open interval in a groundwater well, in meters below land surface.}
  \item{TopOpen2}{is not applicable.}
  \item{BotOpen2}{is not applicable.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
IDWR well construction database, accessed on June 29, 2015
}

\seealso{\code{\link{obs.wells.head}}}

\examples{
plot(obs.wells)
str(obs.wells@data)
}

\keyword{datasets}
