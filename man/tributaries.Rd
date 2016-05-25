\name{tributaries}

\alias{tributaries}

\docType{data}

\title{Groundwater Flow in the Tributary Canyons}

\description{
The location and average flow conditions for model boundaries in the major tributary canyons and upper part of the Wood River Valley, south-central Idaho.
}

\usage{tributaries}

\format{
An object of \code{SpatialPolygonsDataFrame} class containing a set of 22 \code{Polygons} and a \code{data.frame} with the following variable:
\describe{
  \item{Name}{is the tributary name.}
  \item{MinLSD}{is the minimum land-surface datum (elevation) along the transect, in meters above the North American Vertical Datum of 1988 (NAVD 88).}
  \item{BdrkDepth}{is the mean saturated thickness along the transect line, in meters; estimated as the distance between the estimated water table and bedrock elevations.}
  \item{TribWidth}{is the width of the tributary canyon, or length of the transect line, in meters.}
  \item{LandGrad}{is the land surface elevation gradient perpendicular to the cross-sectional transect line, a dimensionless quantity.}
  \item{K}{is the hydraulic conductivity, in meters per day.}
  \item{SatArea}{is the estimated saturated cross-sectional area, in square meters; its geometry is represented as the lower-half of an ellipse with width and height equal to \code{TribWidth} and \code{BdrkDepth}, respectively.}
  \item{DarcyFlow}{is the groundwater flow rate, in cubic meters per day, calculated using a \href{http://en.wikipedia.org/wiki/Darcy_law}{Darcian} analysis.}
  \item{BasinArea}{is the land-surface area defined by the basin above the cross-sectional transect line.}
  \item{BasinAreaType}{is a label that describes the relative basin size; where \code{"big"} indicates a basin area greater than 10 square miles (25.9 square kilometers), and \code{"small"} indicates a basin area that is less than this breakpoint value.}
  \item{PrecipRate}{is the mean precipitation rate within the basin area, in meters per day.}
  \item{PrecipFlow}{is the mean precipitation flow rate, in cubic meters per day, calculated by multiplying \code{PrecipRate} by \code{BasinArea}.}
  \item{FlowRatio}{is the ratio of darcy flow rate to precipitation flow rate, or \code{DarcyFlow} divided by \code{PrecipFlow}, a dimensionless quantity.}
  \item{Flow}{is the estimated average volumetric flow rate, in cubic meters per day.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
U.S. Geological Survey, accessed on July 2, 2015;
a Keyhole Markup Language (\href{http://en.wikipedia.org/wiki/Kml}{KML}) file created in \href{http://www.google.com/earth/}{Google Earth} with polygons drawn by hand in areas of known specified flow boundaries.
Transect lines are defined by the polygon boundaries within the extent of alluvium aquifer (see \code{\link{alluvium.extent}} dataset).
The land surface gradient (\code{LandGrad}) was estimated from the \code{\link{land.surface}} dataset.
Hydraulic conductivity (\code{K}) is the average of two geometric means of hydraulic conductivity in the unconfined aquifer taken from table 2 in Bartolino and Adkins (2012).
The U.S. Geologic Survey \href{http://water.usgs.gov/osw/streamstats/}{StreamStats} tool (Ries and others, 2004) was used to delineate the basin area (\code{BasinArea}) and estimate the precipitation rate (\code{PrecipRate}).
See the \file{package-datasets} vignette for the \R code used to calculate the flow estimates (\code{Flow}).
}

\references{
Bartolino, J.R., and Adkins, C.B., 2012, Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho: U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p., available at \url{http://pubs.usgs.gov/sir/2012/5053/}.

Ries, K.G., Steeves, P.A., Coles, J.D., Rea, A.H., and Stewart, D.W., 2004, StreamStats--A U.S. Geological Survey web application for stream information: U.S. Geological Survey Fact Sheet FS-2004-3115, 4 p., available at \url{http://pubs.er.usgs.gov/usgspubs/fs/fs20043115}.
}

\examples{
plot(tributaries, border = "red")
plot(alluvium.extent, add = TRUE)
str(tributaries@data)
}

\keyword{datasets}
