\name{priority.cuts}

\alias{priority.cuts}

\docType{data}

\title{Priority Cuts}

\description{
Priority cut dates applied to Big Wood River above Magic Reservoir and Silver Creek by Water District 37 and 37M at the end of each month.
}

\usage{priority.cuts}

\format{
A \code{data.frame} object with 112 records and the following variables:
\describe{
  \item{YearMonth}{is the year and month during of the priority cut date, with a required date format of \code{YYYYMM}.}
  \item{Pdate_BWR}{is the date of the priority cut applied to Big Wood River above Magic Reservoir by Water District 37.}
  \item{Pdate_SC}{is the date of the priority cut applied to Silver Creek by Water District 37M.}
}
}

\source{
Idaho Department of Water Resources, accessed on November 17, 2014;
compiled priority cut dates in effect at the end of each month, derived from Water District 37 and 37M records
}

\examples{
str(priority.cuts)
}

\keyword{datasets}
