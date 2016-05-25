\name{AddBubbles}

\alias{AddBubbles}

\title{Add Bubble Map to Plot}

\description{
This function can be used to add a bubble map to a plot.
Proportional circle symbols are used to represent spatial point data, where symbol area varies in proportion to an attribute variable.
}

\usage{
AddBubbles(x, y = NULL, z, zlim = NULL, inches = c(0, 0.2),
           scaling = c("perceptual", "mathematical"),
           bg.pos = "red", bg.neg = "blue", fg = NA, lwd = 0.25,
           cex = 0.7, format = NULL, draw.legend = TRUE,
           loc = c("bottomleft", "topleft", "topright", "bottomright"),
           inset = 0.02, breaks = NULL, break.labels = NULL,
           quantile.breaks = FALSE, make.intervals = FALSE,
           title = NULL, subtitle = NULL, add = TRUE)
}

\arguments{
\item{x, y}{\code{numeric}; the x and y coordinates for the centers of the circle symbols. They can be specified in any way which is accepted by \code{xy.coords}.}
\item{z}{\code{numeric}; is the attribute variable.}
\item{zlim}{\code{numeric}; the minimum and maximum \code{z} values that circle symbols are plotted; defaults to the range of the finite values of \code{z}.}
\item{inches}{\code{numeric}; a vector of length 2 specifying the radii limits for the drawn circle symbol.}
\item{scaling}{\code{character}; selects the proportional symbol mapping algorithm to be used; either \code{"perceptual"} or \code{"mathematical"} scaling (Tanimura and others, 2006).}
\item{bg.pos, bg.neg}{\code{character} or \code{function}; the fill color(s) for circle symbols corresponding to positive and negative \code{z} values, respectively.
  A color palette also may be specified.}
\item{fg}{\code{character}; the outer-line color for circle symbols.
  Specify an \code{NA} value to remove the symbols outer line, and a \code{NULL} value to match the outer-line color with the symbols fill color.}
\item{lwd}{\code{numeric}; is the line width for drawing circle symbols.}
\item{cex}{\code{numeric}; the character expansion factor for legend labels.}
\item{format}{\code{character}; the formatting for legend values, see \code{formatC} for options.}
\item{draw.legend}{\code{logical}; if \code{TRUE}, a legend is drawn.}
\item{loc}{\code{character}; the position of the legend in the main plot region: \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote scale location.}
\item{inset}{\code{numeric}; the inset distance of the legend from the margins as a fraction of the main plot region.
  Defaults to 2 percent of the axis range.}
\item{breaks}{\code{numeric}; a set of finite breakpoints for the legend circle symbols.}
\item{break.labels}{\code{character}; a vector of break labels with length equal to \code{breaks}.}
\item{quantile.breaks}{\code{logical}; if \code{TRUE}, \code{breaks} are set to the sample quantiles of \code{z}.}
\item{make.intervals}{\code{logical}; if \code{TRUE}, represent \code{z} within intervals.
  See \code{findInterval} function for details.}
\item{title}{\code{character}; the main title to be placed at the top of the legend.}
\item{subtitle}{\code{character}; a legend subtitle to be placed below the main title.}
\item{add}{\code{logical}; if \code{TRUE}, circle symbols (and an optional legend) are added to an existing plot.}
}

\details{
Symbols are sequentially drawn in decreasing order of circle diameter.
}

\value{
Primarily used for the side-effect of a bubble map drawn on the current graphics device.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\references{
Tanimura, S., Kuroiwa, C., and Mizota, T., 2006, Proportional Symbol Mapping in R: Journal of Statistical Software, v. 15, no. 5, 7 p.
}

\seealso{\code{symbols}}

\examples{
n <- 50L
x <- cbind(runif(n, 1, 10), runif(n, 1, 10))
z <- runif(n, -5000, 10000)
AddBubbles(x, z = z, fg = "green", lwd = 2, title = "Title", loc = "topright",
           breaks=pretty(z, n = 8), add = FALSE)

Pal1 <- colorRampPalette(c("#F4A582", "#CA0020"))
Pal2 <- colorRampPalette(c("#92C5DE", "#0571B0"))
AddBubbles(x, z = z, bg.pos = Pal1, bg.neg = Pal2, add = FALSE)

AddBubbles(x, z = z, bg.pos = Pal1, bg.neg = Pal2, add = FALSE,
           make.intervals = TRUE)

AddBubbles(x, z = runif(n, 10, 10000), title = "Quantiles", bg.pos = topo.colors,
           quantile.breaks = TRUE, fg = NULL, add = FALSE)
}

\keyword{hplot}
