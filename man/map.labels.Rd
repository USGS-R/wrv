\name{map.labels}

\alias{map.labels}

\docType{data}

\title{Map Labels}

\description{
Map labels in the Wood River Valley and surrounding areas.
}

\usage{map.labels}

\format{
An object of \code{SpatialPointsDataFrame} class containing 40 points with the following variables:
\describe{
  \item{label}{is the text to be written.}
  \item{cex}{is the character expansion factor.}
  \item{col, font}{is the color and font to be used.}
  \item{srt}{is the string rotation in degrees.}
}
Geographic coordinates are in units of meters, in conformance with the North American Datum of 1983 (NAD 83), and placed in the
Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
}

\source{
Best estimates of map label locations.
}

\examples{
plot(map.labels, col = "red")
lab <- cbind(map.labels@coords, map.labels@data)
for (i in seq_len(nrow(lab))) {
  text(lab$x[i], lab$y[i], labels = lab$label[i], cex = lab$cex[i],
       col = lab$col[i], font = lab$font[i], srt = lab$srt[i])
}}

\keyword{datasets}
