\name{et}

\alias{et}

\docType{data}

\title{Evapotranspiration}

\description{
Evapotranspiration (ET) in the Wood River Valley and surrounding areas.
Defined as the amount of water lost to the atmosphere via direct evaporation, transpiration by vegetation, or sublimation from snow covered areas.
}

\usage{et}

\format{
An object of \code{RasterStack} class containing 192 \code{RasterLayer} objects, one layer for each month in the 1995-2010 time period.
Each cell on a layers surface grid represents the monthly depth of ET in meters.
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Idaho Department of Water Resources, accessed on November 17, 2014
}

\seealso{\code{\link{et.method}}}

\examples{
print(et)
plot(et[["199505"]])
}

\keyword{datasets}
