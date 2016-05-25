\name{comb.sw.irr}

\alias{comb.sw.irr}

\docType{data}

\title{Combined Surface-Water Irrigation Diversions}

\description{
Supplemental groundwater rights and associated surface-water rights.
}

\usage{comb.sw.irr}

\format{
A \code{data.frame} object with 1,213 records and the following variables:
\describe{
  \item{WaterRight}{is the name of the supplemental groundwater right.}
  \item{CombWaterRight}{is the name of the surface-water right that shares a combined limit with the groundwater right.}
  \item{Source}{is the river or stream source name for the surface-water right.}
  \item{WaterUse}{is the authorized beneficial use for the surface-water right.}
  \item{MaxDivRate}{is the authorized maximum diversion rate for the surface-water right, in cubic meters per day.}
  \item{Pdate}{is the priority date of the surface-water right.}
}
}

\source{
Idaho Department of Water Resources (IDWR), accessed on April 25, 2014;
derived from combined limit comments in IDWR water rights database.
}

\examples{
str(comb.sw.irr)
}

\keyword{datasets}
