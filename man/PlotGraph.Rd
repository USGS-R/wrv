\name{PlotGraph}

\alias{PlotGraph}

\title{Plot Method for Graphs}

\description{
This function draws a sequence of points, lines, or box-and-whiskers using specified coordinates.
}

\usage{
PlotGraph(x, y, xlab, ylab, asp = NA, xlim = NULL, ylim = NULL,
          xn = 5L, yn = 5L, ylog = FALSE, type = "s", lty = 1, lwd = 1,
          pch = NULL, col = NULL, bg = NA, fill = NULL, pt.cex = 1,
          seq.date.by = "year", scientific = NA, conversion.factor = NULL,
          boxwex = 0.8, center.date.labels = FALSE, bg.polygon = NULL)
}

\arguments{
\item{x, y}{\code{Date}, \code{numeric}, \code{matrix}, or \code{data.frame}; vectors or matrices of data for plotting.
  The vector length or number of rows should match.
  If \code{y} is missing, then \code{x = x[, 1]} and \code{y = x[, 2:n]}.}
\item{xlab}{\code{character}; title for \emph{x} and axis.}
\item{ylab}{\code{character}; a vector of length 2 giving the title for the 1st and 2nd \emph{y} axes.
  The title for the 2nd \emph{y} axis is optional and requires \code{conversion.factor} be specified.}
\item{asp}{\code{numeric}; the \emph{y/x} aspect ratio.}
\item{xlim}{\code{numeric} or \code{Date}; the \code{x} limits \code{(x1, x2)} of the plot.}
\item{ylim}{\code{numeric}; the \code{y} limits \code{(y1, y2)} of the plot.}
\item{xn, yn}{\code{integer}; the desired number of intervals between tick-marks on the \emph{x}- and \emph{y}-axis, respectively.}
\item{ylog}{\code{logical}; if \code{TRUE}, a logarithm scale is used for the \emph{y} axis.}
\item{type}{\code{character}; is the type of plot for each column of \code{y}, see \code{plot} for possible types.
  A box-and-whisker plot is drawn when \code{type = "box"}, with whiskers extending to the data extremes.}
\item{lty}{\code{integer}; is the line type, see \code{par} for all possible types.
  Line types are used cyclically.}
\item{lwd}{\code{numeric}; is the line width.}
\item{pch}{\code{integer}; is the point type, see \code{points} for all possible types.
  Point types are used cyclically.}
\item{col}{\code{character} or \code{function}; is the point or line color, see \code{par} for all possible ways this can be specified.
  Colors are used cyclically.}
\item{bg}{\code{character}; a vector of background colors for the open plot symbols given by \code{pch = 21:25} as in \code{points}.}
\item{fill}{\code{character}; a vector of fill colors for areas beneath (or above, direction towards 0) lines of type \code{"l"} or \code{"s"}.}
\item{pt.cex}{\code{numeric}; expansion factor for the points.}
\item{seq.date.by}{\code{character}, \code{numeric}, or \code{difftime}; is the increment of the date sequence, see \code{seq.Date} for all possible ways this can be specified.}
\item{scientific}{\code{logical}; a vector of length 3 that indicates if axes labels should be encoded in nice scientific format.
  Vector elements correspond to the \code{x}-axis, \code{y}-axis, and second \code{y}-axis labels.
  Values are recycled as necessary.}
\item{conversion.factor}{\code{numeric}; a conversion factor for the 2nd \emph{y} axis.}
\item{boxwex}{\code{numeric}; a scale factor to be applied to all boxes, only applicable for box-and-whisker plots.}
\item{center.date.labels}{\code{logical}; if \code{TRUE}, date labels are horizontally centered between \emph{x}-axis tickmarks.}
\item{bg.polygon}{\code{list}; if specified, a background polygon is drawn.
  The polygon is described using a list of arguments supplied to the \code{polygon} function.
  Passed arguments include \code{"x"} and \code{"col"}.}
}

\value{
Used for the side-effect of a new plot generated.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{matplot}, \code{boxplot}}

\examples{
n <- 50L

x <- as.Date("2008-07-12") + 1:n
y <- sample.int(n, replace = TRUE)
PlotGraph(x, y, ylab = paste("Random number in", c("meters", "feet")), type = "p",
          pch = 16, seq.date.by = "weeks", scientific = FALSE, conversion.factor = 3.28)

graphics.off()
y <- data.frame(lapply(1:3, function(i) sample(n, replace = TRUE)))
PlotGraph(x, y, ylab = "Random number", type = "s", pch = 1, seq.date.by = "days",
          scientific=TRUE)

graphics.off()
y <- sapply(1:3, function(i) sample((1:100) + i * 100, n, replace = TRUE))
m <- cbind(as.numeric(x), y)
col <- c("red", "gold", "green")
PlotGraph(m, xlab = "Number", ylab = "Random number", type = "b", pch = 15:17,
          col = col, pt.cex = 0.9)
legend("topright", LETTERS[1:3], inset = 0.05, col = col, lty = 1, pch = 15:17,
       pt.cex = 0.9, cex = 0.8, bg = "white")

graphics.off()
}

\keyword{hplot}
