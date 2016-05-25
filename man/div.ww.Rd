\name{div.ww}

\alias{div.ww}

\docType{data}

\title{Wastewater Treatment Plant Diversions}

\description{
Discharge from wastewater treatment plants.
}

\usage{div.ww}

\format{
A \code{data.frame} object with 1,182 records and the following variables:
\describe{
  \item{YearMonth}{is the year and month during which diversions were recorded, with a required date format of \code{YYYYMM}.}
  \item{Return}{is the name of the wastewater treatment plant.}
  \item{Reach}{is the name of the river subreach to which treated effluent is discharged; only applicable to wastewater treatment plants that discharge to the river.}
  \item{BigReach}{is the name of the river reach to which treated effluent is discharged; only applicable to wastewater treatment plants that discharge to the river.}
  \item{EntityName}{is the name of the irrigation entity served by the wastewater treatment plant.}
  \item{WWDiv}{is the volume of wastewater discharged during the month, in cubic meters.}
}
}

\source{
Idaho Department of Water Resources and U.S. Geological Survey, accessed on August 11, 2014;
compiled data records from the U.S. Environmental Protection Agency for plants that discharge to the river, and from records of the Idaho Department of Environmental Quality for plants that discharge to land application.
}

\examples{
str(div.ww)
}

\keyword{datasets}
