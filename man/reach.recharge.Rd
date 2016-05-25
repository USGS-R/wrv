\name{reach.recharge}

\alias{reach.recharge}

\docType{data}

\title{Stream-Aquifer Flow Exchange Along River Reaches}

\description{
Stream-aquifer flow exchange along river reaches specified as aquifer recharge.
Values used as observations in parameter estimation.
}

\usage{reach.recharge}

\format{
A \code{data.frame} object with 192 records and the following variables:
\describe{
  \item{YearMonth}{is the year and month of the measurement record, with a required date format of \code{YYYYMM}.}
  \item{nKet_Hai}{the stream-aquifer flow exchange in the Big Wood River, near Ketchum to Hailey river reach, in cubic meters per day.}
  \item{Hai_StC}{the stream-aquifer flow exchange in the Big Wood River, Hailey to Stanton Crossing river reach, in cubic meters per day.}
  \item{WillowCr}{the stream-aquifer flow exchange in the Willow Creek river reach, in cubic meters per day.}
  \item{SilverAbv}{the stream-aquifer flow exchange in Silver Creek, above Sportsman Access river reach, in cubic meters per day.}
  \item{SilverBlw}{the stream-aquifer flow exchange in Silver Creek, Sportsman Access to near Picabo river reach, in cubic meters per day.}
}
}

\details{
A positive stream-aquifer flow exchange indicates aquifer recharge (a losing river reach).
}

\source{
Calculated from continuous stream flow measurements, diversion data, return flow data, and exchange well data
using a flow difference method to estimate groundwater inflows and outflows along a river reach, accessed on September 1, 2015.
Derived from U.S. Geological Survey, Idaho Power Company, and Water District 37 and 37M records.
}

\examples{
str(reach.recharge)
}

\keyword{datasets}
