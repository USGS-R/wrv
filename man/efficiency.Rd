\name{efficiency}

\alias{efficiency}

\docType{data}

\title{Irrigation Efficiency}

\description{
The irrigation efficiency of irrigation entities.
}

\usage{efficiency}

\format{
A \code{data.frame} object with 88 records and the following variables:
\describe{
  \item{EntityName}{is the name of the irrigation entity which the irrigation efficiency is applied.}
  \item{Eff}{is the estimated irrigation efficiency, the ratio of the amount of water consumed by the crop to the amount of water supplied through irrigation.}
  \item{Comment}{a brief comment on irrigation conditions.}
}
}

\source{
Idaho Department of Water Resources, accessed on July 9, 2015
}

\examples{
str(efficiency)
}

\keyword{datasets}
