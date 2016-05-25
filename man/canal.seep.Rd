\name{canal.seep}

\alias{canal.seep}

\docType{data}

\title{Canal Seepage}

\description{
Canal seepage as a fraction of diversions for irrigation entities in the Wood River Valley.
}

\usage{canal.seep}

\format{
A \code{data.frame} object with 19 records and the following variables:
\describe{
  \item{EntityName}{is the name of the irrigation entity served by the canal system.}
  \item{SeepFrac}{is the estimated canal seepage as a fraction of diversions.}
}
}

\source{
Idaho Department of Water Resources, accessed on November 4, 2015
}

\seealso{\code{\link{canals}}}

\examples{
str(canal.seep)

d <- canal.seep[order(canal.seep$SeepFrac, decreasing=TRUE), ]
par(mar = c(4.1, 8.1, 0.1, 0.6))
barplot(d$SeepFrac, names.arg = d$EntityName, horiz = TRUE, cex.names = 0.7,
        cex.axis = 0.7, cex.lab = 0.7, las = 1, xlab = "Seepage fraction")

graphics.off()
}

\keyword{datasets}
