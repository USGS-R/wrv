\name{AddScaleBar}

\alias{AddScaleBar}

\title{Add Scale Bar to Plot}

\description{
This function can be used to add a scale bar to a plot.
}

\usage{
AddScaleBar(asp = 1, unit = NULL, is.lonlat = FALSE,
            loc = c("bottomleft", "topleft", "topright", "bottomright"),
            offset = c(0, 0), lab.vert.exag = NULL)
}

\arguments{
\item{asp}{\code{numeric}; the \emph{y/x} aspect ratio for spatial axes.}
\item{unit}{\code{character}; axis unit of measurement, for example \code{"METERS"}.}
\item{is.lonlat}{\code{logical}; if \code{TRUE}, plot coordinates are in longitude and latitude.}
\item{loc}{\code{character}; the position of the scale bar in the plot region: \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote scale location.}
\item{offset}{\code{numeric}; the \code{x} and \code{y} adjustments of the scale bar, in inches.}
\item{lab.vert.exag}{\code{logical}; if \code{TRUE}, a label is drawn specifying the vertical exaggeration.}
}

\value{
Used for the side-effect of a scale bar drawn on the current graphics device.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{\link{PlotCrossSection}}, \code{\link{PlotMap}}}

\examples{
plot(-100:100, -100:100, type = "n", xlab = "x", ylab = "y", asp = 2)
AddScaleBar(2, unit = "FEET", loc = "topleft")
AddScaleBar(2, unit = "METERS", loc = "bottomright", offset = c(-0.2, 0))
}

\keyword{hplot}
