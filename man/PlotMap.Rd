\name{PlotMap}

\alias{PlotMap}

\title{Plot Method for Maps}

\description{
This function maps raster layer values. A key showing how the colors map to raster values is shown below the map.
}

\usage{
PlotMap(r, layer = 1, att = NULL, n, breaks, xlim = NULL, ylim = NULL,
        zlim = NULL, asp = 1, extend.xy = FALSE, extend.z = FALSE,
        reg.axs = TRUE, trim.r = TRUE, dms.tick = FALSE, bg.lines = FALSE,
        bg.image = NULL, bg.image.alpha = 1, pal = NULL, col = NULL,
        max.dev.dim = c(43, 56), labels = NULL, scale.loc = "bottomleft",
        arrow.loc = NULL, explanation = NULL, credit = proj4string(r),
        shade = NULL, contour.lines = NULL, rivers = NULL, lakes = NULL,
        roads = NULL, draw.key = NULL, draw.raster = TRUE, useRaster)
}

\arguments{
\item{r}{\code{RasterLayer}, \code{SpatialGridDataFrame}, or \code{CRS}; a raster layer with values to be plotted or a coordinate reference system (CRS).}
\item{layer}{\code{integer}; the column to use in the \code{SpatialGridDataFrame}.}
\item{att}{\code{numeric} or \code{character}; the variable identifying the levels attribute to use in the Raster Attribute Table (RAT).
  This argument requires \code{r} values that are of class \code{factor}.}
\item{n}{\code{integer}; the desired number of intervals to partition the range of raster values (or \code{zlim} if specified) (optional).}
\item{breaks}{\code{numeric}; a vector of break points used to partition the colors representing numeric raster values (optional).}
\item{xlim}{\code{numeric}; a vector of length 2 giving the minimum and maximum values for the \emph{x}-axis.}
\item{ylim}{\code{numeric}; a vector of length 2 giving the minimum and maximum values for the \emph{y}-axis.}
\item{zlim}{\code{numeric}; a vector of length 2 giving the minimum and maximum raster values for which colors should be plotted.}
\item{asp}{\code{numeric}; the \emph{y/x} aspect ratio for spatial axes.}
\item{extend.xy}{\code{logical}; if \code{TRUE}, the spatial limits will be extended to the next tick mark on the axes beyond the grid extent.}
\item{extend.z}{\code{logical}; if \code{TRUE}, the raster value limits will be extended to the next tick mark on the color key beyond the measured range.}
\item{reg.axs}{\code{logical}; if \code{TRUE}, the spatial data range is extended.}
\item{trim.r}{\code{logical}; if \code{TRUE}, the outer rows and columns that consist of all \code{NA} values will be removed.}
\item{dms.tick}{\code{logical}; if \code{TRUE}, the axes tickmarks are specified in degrees, minutes, and decimal seconds.}
\item{bg.lines}{\code{logical}; if \code{TRUE}, the graticule is drawn in back of the raster layer using white lines and a grey background.}
\item{bg.image}{\code{RasterLayer}; an image to drawn in back of the main raster layer \code{r}.}
\item{bg.image.alpha}{\code{numeric}; the opacity of the background image from 0 to 1.}
\item{pal}{\code{function}; a color palette to be used to assign colors in the plot, \code{rainbow} by default.}
\item{col}{\code{character}; a vector of colors to be used in the plot.
  This argument requires \code{breaks} specification for numeric values of \code{r} and overrides any palette function specification.
  For numeric values there should be one less color than breaks. Factors require a color for each level.}
\item{max.dev.dim}{\code{numeric}; a vector of length 2 giving the maximum width and height for the graphics device in picas, respectively.
  Suggested dimensions for single-column, double-column, and sidetitle figures are \code{c(21, 56)}, \code{c(43, 56)}, and \code{c(56, 43)}, respectively.}
\item{labels}{\code{list}; describes the location and values of labels in the color key.
  This list may include components \code{at} and \code{labels}.}
\item{scale.loc}{\code{character}; the position of the scale bar: \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote scale location.}
\item{arrow.loc}{\code{character}; the position of the north arrow: \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote arrow location.}
\item{explanation}{\code{character}; a label explaining the raster value.}
\item{credit}{\code{character}; a label crediting the base map.}
\item{shade}{\code{list}; if specified, a semi-transparent shade layer is drawn on top of the raster layer.
  This layer is described using a list of arguments supplied to \code{hillShade}.
  Passed arguments include \code{"angle"} and \code{"direction"}.
  Additional arguments also may be passed that control the vertical aspect ratio (\code{"z.factor"}) and color opacity (\code{"alpha"}).}
\item{contour.lines}{\code{list}; if specified, contour lines are drawn.
  The contours are described using a list of arguments supplied to \code{contour}.
  Passed arguments include \code{"drawlables"}, \code{"method"}, and \code{"col"}.}
\item{rivers}{\code{list}; if specified, lines are drawn.
  The lines are described using a list of arguments supplied to the plot method for \code{SpatialLines}.
  Passed arguments include \code{"x"}, \code{"col"}, and \code{"lwd"}.}
\item{lakes}{\code{list}; if specified, polygons are drawn.
  The polygons are described using a list of arguments supplied to the plot method for \code{SpatialPolygons}.
  Passed arguments include \code{"x"}, \code{"col"}, \code{"border"}, and \code{"lwd"}.
  Bitmap images require a regular grid.}
\item{roads}{\code{list}; if specified, lines are drawn.
  The lines are described using a list of arguments supplied to the plot method for \code{SpatialLines}.
  Passed arguments include \code{"x"}, \code{"col"}, and \code{"lwd"}.}
\item{draw.key}{\code{logical}; indicates if a color key should be drawn.}
\item{draw.raster}{\code{logical}; if \code{FALSE}, the raster image is not drawn.}
\item{useRaster}{\code{logical}; if \code{TRUE}, a bitmap raster is used to plot \code{r} instead of polygons.
  If \code{UseRaster} is not specified, raster images are used when the \code{getOption("preferRaster")} is true.}
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

\seealso{\code{\link{AddScaleBar}}, \code{\link{AddColorKey}}}

\examples{
r <- raster(system.file("external/test.grd", package="raster"))
PlotMap(r, scale.loc = "topleft", dms.tick = TRUE, trim.r = TRUE)

graphics.off()
r <- raster(nrow = 10, ncol = 10)
r[] <- 1L
r[51:100] <- 2L
r[3:6, 1:5] <- 8L
r <- ratify(r)
rat <- levels(r)[[1]]
rat$land.cover <- c("Pine", "Oak", "Meadow")
rat$code <- c(12, 25, 30)
levels(r) <- rat
PlotMap(r, att = "land.cover", col = c("grey", "orange", "purple"))
PlotMap(r, att = "code")

graphics.off()
r <- alluvium.thickness
PlotMap(r@crs, bg.image = hill.shading, reg.axs = FALSE)
plot(alluvium.extent, border = "red", add = TRUE)
PlotMap(r, bg.image = hill.shading, bg.image.alpha = 0.6)
PlotMap(r, n = 10, extend.xy = TRUE)

graphics.off()
PlotMap(r, ylim = c(NA, 1360000), max.dev.dim = c(56, 43), n = 10, extend.z = TRUE,
        contour.lines = list(col = "#A9A9A9"))
plot(alluvium.extent, add = TRUE)
shade <- list(z.factor = 15, alpha = 0.4)
txt <- "Land surface elevation in meters above National Geodetic Vertical Datum of 1929."
ans <- PlotMap(r, ylim = c(NA, 1360000), max.dev.dim = c(56, 43), bg.lines = TRUE,
               shade = shade, arrow.loc = "topright", explanation = txt)

graphics.off()
}

\keyword{hplot}
