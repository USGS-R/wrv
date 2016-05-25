\name{precipitation}

\alias{precipitation}

\docType{data}

\title{Precipitation Rate}

\description{
Precipitation rates in the Wood River Valley and surrounding areas.
}

\usage{precipitation}

\format{
A \code{data.frame} object with 582 records and the following variables:
\describe{
  \item{YearMonth}{is the year and month during which precipitation were recorded, with a required date format of \code{YYYYMM}.}
  \item{PrecipZone}{the name of the precipitation zone, see \code{\link{precip.zones}} dataset for details.}
  \item{Precip}{is the monthly depth of precipitation accounting for spring melt, in meters.}
  \item{Precip.raw}{is the monthly depth of precipitation recorded at the weather station, in meters.}
}
}

\source{
Idaho Department of Water Resources, accessed on April 24, 2015
}

\seealso{\code{\link{precip.zones}}, \code{\link{swe}}}

\references{
National Oceanic and Atmospheric Administration's National Weather Service (\href{http://www.nws.noaa.gov/om/coop/}{NWS}) Cooperative Observer Program

U.S. Bureau of Reclamation's Cooperative Agricultural Weather Network (\href{http://www.usbr.gov/pn/agrimet/}{AgriMet})
}

\examples{
str(precipitation)

d <- precipitation
d <- data.frame(Date = as.Date(paste0(d$YearMonth, "15"), format = "\%Y\%m\%d"),
                Precip = d$Precip)
zones <- c("Hailey", "Ketchum", "Picabo")
d1 <- d[precipitation$PrecipZone == zones[1], ]
d2 <- d[precipitation$PrecipZone == zones[2], ]
d3 <- d[precipitation$PrecipZone == zones[3], ]
d <- merge(merge(d1, d2, by = "Date"), d3, by = "Date")

col <- c("red", "blue", "green")
ylab <- paste("Precipitation in", c("meters", "feet"))
PlotGraph(d, ylab = ylab, col = col, lty = 1:3, conversion.factor = 3.28084)
legend("topright", zones, col = col, lty = 1:3, inset = 0.02, cex = 0.7,
       box.lty = 1, bg = "#FFFFFFE7")

graphics.off()
}

\keyword{datasets}
