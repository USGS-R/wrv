\name{GetSeasonalMult}

\alias{GetSeasonalMult}

\title{Get Seasonal Multiplier}

\description{
This function determines the seasonal fraction of the long-term mean value.
}

\usage{
GetSeasonalMult(x, reduction, d.in.mv.ave, tr.stress.periods)
}

\arguments{
\item{x}{\code{data.frame}; a time series with \code{Date} and \code{numeric} components.}
\item{reduction}{\code{numeric}; is the signal amplitude reduction, a dimensionless quantity.
  Its magnitude should be greater than or equal to 1, where a value of 1 indicates no reduction in the signal amplitude.}
\item{d.in.mv.ave}{\code{numeric}; is the number of days in the moving average subset.}
\item{tr.stress.periods}{\code{Date}; a vector giving the start and end dates for each model stress period.}
}

\details{
A simple moving average is first calculated for each month using the previous data (such as the previous 9-months of stage data recorded at a streamgage).
The seasonal average of the monthly moving average is then passed through a signal amplitude reduction algorithm.
The reduced values are then divided by the mean of the seasonal reduced data to give the seasonal fraction of the mean (seasonal multiplier).
}

\value{
An object of \code{data.frame} class with \code{Date} and \code{numeric} components; that is, the starting date and multiplier for each season.
}

\author{
J.C. Fisher and J.R. Bartolino, U.S. Geological Survey, Idaho Water Science Center

A.H. Wylie and J. Sukow, Idaho Department of Water Resources
}

\examples{
tr.interval <- as.Date(c("1995-01-01", "2011-01-01"))
tr.stress.periods <- seq(tr.interval[1] , tr.interval[2], "1 month")
m <- GetSeasonalMult(gage.disch[, c("Date", "13139510")], 2, 273.932, tr.stress.periods)
f <- vapply(tributaries$Flow, function(i) m$multiplier * i, rep(0, nrow(m)))
colnames(f) <- tributaries$ID
d <- cbind(m, f)
str(d)
}

\keyword{manip}
