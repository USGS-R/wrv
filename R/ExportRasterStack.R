ExportRasterStack <- function(rs, path, zip="",
                              col=rainbow(250, start=0.0, end=0.8)) {

  dir.create(path, showWarnings=FALSE, recursive=TRUE)

  dir.create(path.csv <- file.path(path, "csv"), showWarnings=FALSE)
  dir.create(path.png <- file.path(path, "png"), showWarnings=FALSE)
  dir.create(path.tif <- file.path(path, "tif"), showWarnings=FALSE)
  dir.create(path.rda <- file.path(path, "rda"), showWarnings=FALSE)
  dir.create(path.kml <- file.path(path, "kml"), showWarnings=FALSE)

  n <- 0L
  for (i in names(rs)) {
    n <- n + 1L
    fig.num <- formatC(n, width=2, format="d", flag="0")

    f <- file.path(path.csv, paste(fig.num, "_", i, ".csv", sep=""))
    m <- matrix(data=rs[[i]][], nrow=nrow(rs), ncol=ncol(rs), byrow=TRUE)
    write.table(m, file=f, quote=FALSE, sep=",", na="", row.names=FALSE,
                col.names=FALSE, qmethod="double")

    f <- file.path(path.png, paste(fig.num, "_", i, ".png", sep=""))
    png(filename=f, width=7, height=7, units="in", pointsize=12, res=1200,
        antialias="cleartype")
    plot(rs[[i]], maxpixels=length(rs[[i]]), col=col, main=names(rs[[i]]),
         asp=1)
    dev.off()

    f <- file.path(path.tif, paste(fig.num, "_", i, ".tif", sep=""))
    writeRaster(rs[[i]], filename=f, format="GTiff", overwrite=TRUE,
                NAflag=-999)
  }

  base.name <- "raster"

  f <- file.path(path.rda, "rasters.rda")
  save(rs, file=f)

  f <- file.path(path.kml, "rasters.kml")
  crs <- "+proj=longlat +datum=WGS84"
  rs <- projectRaster(rs, crs=crs, method="ngb", alignOnly=FALSE)
  KML(rs, f, col=col, maxpixels=ncell(rs) * 2, blur=5, zip=zip,
      overwrite=TRUE)

  invisible()
}
