\name{canals}

\alias{canals}

\docType{data}

\title{Canal Systems}

\description{
The canal systems in the Wood River Valley and surrounding areas.
}

\usage{canals}

\format{
An object of \code{SpatialLinesDataFrame} class containing 113 \code{Lines} and a \code{data.frame} with the following variable:
\describe{
  \item{EntityName}{the name of the irrigation entity served by the canal system.}
  \item{Name}{the local canal name.}
}
}

\source{
Idaho Department of Water Resources, accessed on November 29, 2014
}

\seealso{\code{\link{r.canals}}, \code{\link{canal.seep}}}

\examples{
plot(canals, col = "#3399CC")
str(canals@data)
}

\keyword{datasets}
