\name{AddInsetMap}

\alias{AddInsetMap}

\title{Add Inset Map to Plot}

\description{
This function can be used to add an inset map to a plot.
}

\usage{
AddInsetMap(p, col = c("#D8D8D8", "#BFA76F"),
            main.label = list(label = NA, adj = NULL),
            sub.label = list(label = NA, adj = NULL),
            loc = c("bottomleft", "topleft", "topright", "bottomright"),
            inset = 0.02, width = NULL)
}

\arguments{
\item{p}{\code{SpatialPolygons}; the polygon describing the large map.}
\item{col}{\code{character}; a vector of length 2 giving the colors for filling the large map polygon \code{p} and the smaller plot extent rectangle.}
\item{main.label}{\code{list}; a list with components \code{label} and \code{adj}.
  The text label and position (\code{x} and \code{y} adjustment of the label) for the large map, respectively.}
\item{sub.label}{\code{list}; identical to the \code{main.label} argument but for the plot extent rectangle.}
\item{loc}{\code{character}; the position of the inset map in the main plot region: \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote scale location.}
\item{inset}{\code{numeric}; the inset distance from the margins as a fraction of the main plot region.
  Defaults to 2 percent of the axis range.}
\item{width}{\code{numeric}; the width of the inset map, in inches.}
}

\details{
The smaller axis-aligned rectangle (relative to the larger map polygon) is defined by the user coordinate extent of the main plot region, see \code{par("usr")}.
}

\value{
Used for the side-effect of a inset map drawn on the current graphics device.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{\link{PlotMap}}}

\examples{
PlotMap(alluvium.thickness@crs, bg.image = hill.shading, reg.axs = FALSE)
AddInsetMap(idaho, main.label = list("IDAHO", adj = c(-0.4, -4.9)),
            sub.label = list("Map area", adj = c(0.5, 2.5)), loc = "topright")

graphics.off()
}

\keyword{hplot}
