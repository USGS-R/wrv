\name{GetDaysInMonth}

\alias{GetDaysInMonth}

\title{Get Number of Days in a Year and Month}

\description{
This function determines the number of days in a year and month.
}

\usage{
GetDaysInMonth(x)
}

\arguments{
\item{x}{\code{character} or \code{integer}; a vector of year and month values, with a required date format of \code{YYYYMM}.}
}

\value{
Returns an integer vector indicating the number of days in each year and month value specified in \code{x}.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\examples{
GetDaysInMonth(c("199802", "199804", "200412"))
}

\keyword{utilities}
