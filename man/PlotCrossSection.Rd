\name{PlotCrossSection}

\alias{PlotCrossSection}

\title{Plot Method for Cross Sections}

\description{
This function creates a cross-section view of raster data.
A key showing how the colors map to raster values is shown below the map.
}

\usage{
PlotCrossSection(transect, rs, geo.lays = names(rs), val.lays = NULL,
                 wt.lay = NULL, asp = 1, ylim = NULL, max.dev.dim = c(43, 56),
                 n = NULL, breaks = NULL, pal = NULL, col = NULL, ylab = NULL,
                 unit = NULL, id = c("A", "A'"), labels = NULL,
                 explanation = NULL, features = NULL, max.feature.dist = Inf,
                 draw.key = TRUE, draw.sep = TRUE, is.categorical = FALSE,
                 contour.lines = NULL, bg.col = "#E1E1E1", wt.col = "#FFFFFFD8")
}

\arguments{
\item{transect}{\code{SpatialLines}; the piecewise linear transect line.}
\item{rs}{\code{RasterStack}; a collection of \code{RasterLayer} objects with the same extent and resolution.}
\item{geo.lays}{\code{character}; a vector of names in \code{rs} that specify the geometry raster layers; these must be given in decreasing order, that is, from the upper most (such as land surface) to the lowest (such as a bedrock surface).}
\item{val.lays}{\code{character}; a vector of names in \code{rs} that specify the value raster layers (optional).
  Values from the first layer are mapped as colors to the area between the first and second geometry layers; the second layer mapped between the second and third geometry layers, and so on.}
\item{wt.lay}{\code{character}; the name in \code{rs} that specifies the water-table raster layer (optional).}
\item{asp}{\code{numeric}; the \emph{y/x} aspect ratio for spatial axes.}
\item{ylim}{\code{numeric}; a vector of length 2 giving the minimum and maximum values for the \emph{y}-axis.}
\item{max.dev.dim}{\code{numeric}; a vector of length 2 giving the maximum width and height for the graphics device in picas, respectively.
  Suggested dimensions for single-column, double-column, and sidetitle figures are \code{c(21, 56)}, \code{c(43, 56)}, and \code{c(56, 43)}, respectively.}
\item{n}{\code{integer}; the desired number of intervals to partition the range of raster values (optional).}
\item{breaks}{\code{numeric}; a vector of break points used to partition the colors representing numeric raster values (optional).}
\item{pal}{\code{function}; a color palette to be used to assign colors in the plot, \code{rainbow} by default.}
\item{col}{\code{character}; a vector of colors to be used in the plot.
  This argument requires \code{breaks} specification for numeric raster values and overrides any palette function specification.
  For numeric values there should be one less color than breaks.
  Categorical data require a color for each category.}
\item{ylab}{\code{character}; a label for the \emph{y} axis.}
\item{unit}{\code{character}; a label for the measurement unit of the \emph{x}- and \emph{y}-axes.}
\item{id}{\code{character}; a vector of length 2 giving the labels for the end points of the transect line, defaults to \emph{A--A'}.}
\item{labels}{\code{list}; describes the location and values of labels in the color key.
  This list may include components \code{at} and \code{labels}, numeric and character vectors, respectively.}
\item{explanation}{\code{character}; a label that describes the cell values.}
\item{features}{\code{SpatialGridDataFrame}; point features adjacent to the transect line that are used as reference labels for the upper geometry layer.}
\item{max.feature.dist}{\code{numeric}; the maximum distance from a point feature to the transect line, specified in the units of the \code{rs} projection.}
\item{draw.key}{\code{logical}; if \code{FALSE}, a color key is not drawn.}
\item{draw.sep}{\code{logical}; if \code{TRUE}, lines separating geometry layers are drawn.}
\item{is.categorical}{\code{logical}; if \code{TRUE}, cell values in \code{val.lays} represent categorical data; otherwise, these data values are assumed continuous.}
\item{contour.lines}{\code{list}; if specified, contour lines are drawn.
  The contours are described using a list of arguments supplied to \code{contour}.
  Passed arguments include \code{"drawlables"}, \code{"method"}, and \code{"col"}.}
\item{bg.col}{\code{character}; the color used for the background of the area below the upper geometry raster layer.}
\item{wt.col}{\code{character}; the color used for the water-table line.}
}

\details{
The dimensions of a new graphics device is dependent on the argument values of \code{max.dev.dim} and \code{asp}.
}

\value{
Used for the side-effect of a new plot generated.
Returns a \code{list} object with the following graphical parameters:
\item{din}{\code{numeric}; the device dimensions \code{(width, height)}, in inches.}
\item{usr}{\code{numeric}; the extremes of the coordinates of the plotting region \code{(x1, x2, y1, y2)}.}
\item{heights}{\code{numeric}; the relative heights on the device \code{(upper, lower)} for the map and color-key plots.}
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{\link{ExtractAlongTransect}}, \code{\link{AddScaleBar}}, \code{\link{AddColorKey}}}

\examples{
data(volcano)
x <- seq(from = 2667405, length.out = 61, by = 10)
y <- seq(from = 6478705, length.out = 87, by = 10)
r1 <- raster(volcano, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y),
             crs = CRS("+init=epsg:27200"))
r2 <- min(r1[]) - r1 / 10
r3 <- r1 - r2
rs <- stack(r1, r2, r3)
names(rs) <- c("r1", "r2", "r3")

xy <- rbind(c(2667508, 6479501), c(2667803, 6479214), c(2667508, 6478749))
transect <- SpatialLines(list(Lines(list(Line(xy)), ID = "Transect")),
                         proj4string = crs(rs))

plot(r1)
lines(transect)
text(as(transect, "SpatialPoints"), labels = c("A", "BEND", "A'"), cex = 0.7,
     pos = c(3, 4, 1), offset = 0.1, font = 4)

graphics.off()
PlotCrossSection(transect, rs, geo.lays = c("r1", "r2"), val.lays = "r3",
                 ylab="Elevation", asp = 5, unit = "METERS",
                 explanation = "Vertical thickness between layers, in meters.")

graphics.off()
}

\keyword{hplot}
