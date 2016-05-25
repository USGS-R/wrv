\name{misc.seepage}

\alias{misc.seepage}

\docType{data}

\title{Recharge from Miscellaneous Seepage Sites}

\description{
Recharge from miscellaneous seepage sites in the Wood River Valley, Idaho.
}

\usage{misc.seepage}

\format{
A \code{data.frame} object with 2 records and the following variables:
\describe{
  \item{RechSite}{is the name of the recharge site, see \code{\link{bellevue.wwtp.ponds}} and \code{\link{bypass.canal}} datasets.}
  \item{199501, \dots, 201012}{is the monthly volume of recharge during a stress period, in cubic meters.
    The variable name is specified as year and month.}
}
}

\source{
Idaho Department of Water Resources, accessed on January 15, 2015
}

\examples{
str(misc.seepage)
}

\keyword{datasets}
