\name{ExportRasterStack}

\alias{ExportRasterStack}

\title{Export Raster Stack}

\description{
This function writes a raster-stack, a collection of raster layers, to local directories using multiple file formats.
}

\usage{
ExportRasterStack(rs, path, zip = "", col = rainbow(250, start = 0.0, end = 0.8))
}

\arguments{
\item{rs}{\code{RasterStack}; a collection of \code{\linkS4class{RasterLayer}} objects with the same extent and resolution.}
\item{path}{\code{character}; path name to write raster stack.}
\item{zip}{\code{character}; if there is no zip program on your path (on windows), you can supply the full path to a \file{zip.exe} here, in order to make a KMZ file.}
\item{col}{\code{character}; a vector of colors.}
}

\details{
Five local directories are created under \code{path} and named after their intended file formats:
Comma-Separated Values (\file{csv}),
Portable Network Graphics (\file{png}),
georeferenced TIFF (\file{tif}),
R Data (\file{rda}),
and Keyhole Markup Language (\file{kml}).
For its reference system, \file{kml} uses geographic coordinates: longitude and latitude components as defined by the World Geodetic System of 1984.
Therefore, the conversion of gridded data between cartographic projections may introduce a new source of error.

To install \file{zip.exe} on windows, download the latest binary version from the \href{http://www.info-zip.org/Zip.html#Downloads}{Info-ZIP} website:
select one of the given FTP locations, enter directory \file{win32}, download \file{zip300xn.zip}, and extract.
}

\value{None. Used for the side-effect files written to disk.}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{writeRaster}}

\examples{\dontrun{
f <- file.path(getwd(), "SIR2016-5080/ancillary/uncalibrated/data/rda/rasters.rda")
load(file = f)
ExportRasterStack(rs, tempdir())}
}

\keyword{IO}
