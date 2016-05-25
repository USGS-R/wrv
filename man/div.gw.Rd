\name{div.gw}

\alias{div.gw}

\docType{data}

\title{Groundwater Diversions}

\description{
Groundwater diversions recorded by Water District 37 or municipal water providers.
Groundwater is diverted from the aquifer by means of either pumping wells or flowing-artesian wells.
}

\usage{div.gw}

\format{
A \code{data.frame} object with 7,292 records and the following variables:
\describe{
  \item{YearMonth}{is the year and month during which diversions were recorded, with a required date format of \code{YYYYMM}.}
  \item{Diversion}{is the name of the well.}
  \item{Reach}{is the name of the river subreach into which the well water is discharged; only applicable to exchange wells.}
  \item{BigReach}{is the name of the river reach into which the well water is discharged; only applicable to exchange wells.}
  \item{EntityName}{is the name of the irrigation entity which the well supplies water.}
  \item{WMISNumber}{is the well number in the Idaho Department of Water Resources (IDWR) Water Measurement Information System.}
  \item{GWDiv}{is the volume of water diverted during the month, in cubic meters.}
}
}

\source{
IDWR, accessed on December 11, 2014;
compiled data records from Water District 37 and 37M, City of Ketchum, Sun Valley Water and Sewer District, City of Hailey, and City of Bellevue.
}

\examples{
str(div.gw)
}

\keyword{datasets}
