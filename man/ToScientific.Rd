\name{ToScientific}

\alias{ToScientific}

\title{Format for Scientific Notation}

\description{
This function formats numbers in scientific notation \eqn{m \times 10^{n}}.
}

\usage{
ToScientific(x, digits = format.info(as.numeric(x))[2],
             lab.type = c("latex", "plotmath"))
}

\arguments{
\item{x}{\code{numeric}; a vector of numbers.}
\item{digits}{\code{integer}; the number of digits after the decimal point for the mantissa.}
\item{lab.type}{\code{character}; by default, LaTeX formatted strings for labels are returned.
  Alternatively, \code{lab.type = "plotmath"} returns \code{plotmath}-compatible expressions.}
}

\value{
For the default \code{lab.type = "latex"}, a \code{character} vector of the same length as argument \code{x}.
And for \code{lab.type = "plotmath"}, an \code{expression} of the same length as \code{x}, typically with elements of the form \code{m x 10^n}.
In order to comply with \href{http://www.section508.gov/}{Section 508},
an \dQuote{\code{x}} is used as the label separator for the \code{plotmath} type---rather than the more common \dQuote{\code{\%*\%}} seperator.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\examples{
x <- c(-1e+09, 0, NA, pi * 10^(-5:5))
ToScientific(x, digits = 2)
ToScientific(x, digits = 2, lab.type = "plotmath")
}

\keyword{utilities}
