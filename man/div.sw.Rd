\name{div.sw}

\alias{div.sw}

\docType{data}

\title{Surface-Water Diversions}

\description{
Surface-water diversions recorded by Water District 37 or municipal water providers.
}

\usage{div.sw}

\format{
A \code{data.frame} object with 15,550 records and the following variables:
\describe{
  \item{YearMonth}{is the year and month during which diversions were recorded, with a required date format of \code{YYYYMM}.}
  \item{Diversion}{is the name of the surface-water diversion.}
  \item{Reach}{is the river subreach from which the water is diverted.}
  \item{BigReach}{is the river reach from which the water is diverted.}
  \item{EntityName}{is the name of the irrigation entity which the diversion supplies water.}
  \item{SWDiv}{is the volume of water diverted during the month, in cubic meters.}
}
}

\source{
Idaho Department of Water Resources, accessed on December 11, 2014;
compiled data records from Water District 37 and 37M, City of Hailey, City of Bellevue, City of Ketchum, and Sun Valley Water and Sewer District.
}

\examples{
str(div.sw)
}

\keyword{datasets}
