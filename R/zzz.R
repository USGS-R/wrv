.onAttach <- function(lib, pkg) {
  if (interactive()) {
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    msg <- "USGS Research Package: https://owi.usgs.gov/R/packages.html#research"
    packageStartupMessage(paste(strwrap(msg), collapse="\n"))
  }
  raster::rasterOptions(standardnames=FALSE)
  invisible()
}
