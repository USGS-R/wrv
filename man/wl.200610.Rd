\name{wl.200610}

\alias{wl.200610}

\docType{data}

\title{Groundwater-Level Contours for October 2006}

\description{
Groundwater-level contours with a 20 foot (6.096 meter) contour interval for the unconfined aquifer in the Wood River Valley, south-central Idaho, representing conditions during October 2006.
}

\usage{wl.200610}

\format{
An object of \code{SpatialLinesDataFrame} class containing 265 \code{Lines} and a \code{data.frame} with the following variables:
\describe{
  \item{CONTOUR}{is the groundwater elevation contour value in meters above the North American Vertical Datum of 1988 (NAVD 88).}
  \item{certainty}{is the certainty of the groundwater-level contour based on data position and density, specified as \code{"good"} or \code{"poor"}.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
This dataset is from Plate 1 in Skinner and others (2007), and available on the \href{http://water.usgs.gov/GIS/metadata/usgswrd/XML/sir2007-5258_oct2006wl.xml}{WRD NSDI Node}.
}

\references{
Skinner, K.D., Bartolino, J.R., and Tranmer, A.W., 2007, Water-resource trends and comparisons between partial development and October 2006 hydrologic conditions, Wood River Valley, south-central, Idaho: U.S. Geological Survey Scientific Investigations Report 2007-5258, 30 p., available at \url{http://pubs.usgs.gov/sir/2007/5258/}
}

\examples{
is.good <- wl.200610@data$certainty == "good"
plot(wl.200610[is.good, ], col = "blue")
plot(wl.200610[!is.good, ], col = "red", lty = 2, add = TRUE)
str(wl.200610@data)
}

\keyword{datasets}
