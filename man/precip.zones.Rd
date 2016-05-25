\name{precip.zones}

\alias{precip.zones}

\docType{data}

\title{Precipitation Zones}

\description{
Precipitation zones specified for the Wood River Valley and surrounding areas.
There are three precipitation zones, each containing a single weather station.
Precipitation zones were distributed to maintain the geographic similarity between weather stations and zones.
}

\usage{precip.zones}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing 3 \code{Polygons} and a \code{data.frame} with the following variables:
\describe{
  \item{ID}{a numeric identifier assigned to the polygon.}
  \item{PrecipZone}{the name of the precipitation zone:
    \dQuote{Ketchum}, the northernmost zone with data from the Ketchum National Weather Service coop weather station.
    \dQuote{Hailey}, the central zone with data from the Hailey 3NNW National Weather Service coop weather station.
    \dQuote{Picabo}, the southernmost zone with data from the Picabo AgriMet weather station.
  }
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Created using the northing midpoint between weather stations, see \code{\link{weather.stations}} dataset.
}

\seealso{\code{\link{precipitation}}}

\examples{
col <- c("#D1F2A5", "#FFC48C", "#F56991")
plot(precip.zones, col = col)
legend("topright", legend = precip.zones@data$PrecipZone, fill = col, bty = "n")
plot(alluvium.extent, add = TRUE)

print(precip.zones)
}

\keyword{datasets}
