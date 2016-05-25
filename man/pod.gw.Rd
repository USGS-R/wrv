\name{pod.gw}

\alias{pod.gw}

\docType{data}

\title{Points of Diversion for Groundwater}

\description{
Points of diversion for groundwater within the Wood River Valley model study area.
}

\usage{pod.gw}

\format{
A \code{data.frame} object with 1,755 records and the following variables:
\describe{
  \item{WMISNumber}{is a unique number assigned to a water right point of diversion.}
  \item{WaterRight}{is a number identifying a specific authorization to use water in a prescribed manner.}
  \item{EntityName}{is the name of the irrigation entity the point of diversion is assigned to.}
  \item{EntitySrce}{is the source of water for an irrigation entity.
    Possible sources of water include surface water, groundwater and mixed.
    Mixed source entities derive water from both groundwater and surface water.}
  \item{Pdate}{is the priority date, the date the water right was established.}
  \item{IrrRate}{is the irrigation rate in cubic meters per day, the maximum permitted water use rate associated with a water right.}
}
}

\source{
Idaho Department of Water Resources water rights database, accessed on December 11, 2014
}

\seealso{\code{\link{pod.wells}}}

\examples{
summary(pod.gw)
}

\keyword{datasets}
