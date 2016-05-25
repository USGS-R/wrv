\name{irr.lands.year}

\alias{irr.lands.year}

\docType{data}

\title{Irrigation Lands for a Given Year}

\description{
The annual land classification for irrigation practices is only available for select years.
For missing years, this dataset provides substitute years when land-classification was available (see \code{\link{irr.lands}}).
}

\usage{irr.lands.year}

\format{
A \code{data.frame} object with 16 records and the following variables:
\describe{
  \item{Year}{is the year with a required date format of \code{YYYY}.}
  \item{IL_Year}{is the substitute year with a required date format of \code{YYYY}.}
}
}

\source{
Idaho Department of Water Resources, accessed on April 25, 2014
}

\examples{
str(irr.lands.year)
}

\keyword{datasets}
