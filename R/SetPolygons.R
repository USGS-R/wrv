SetPolygons <- function(x, y, cmd=c("gIntersection", "gDifference"),
                        buffer.width=NA) {

  cmd <- match.arg(cmd)

  if (!inherits(x, c("SpatialPolygons", "SpatialPolygonsDataFrame")))
    stop("argument 'x' is the wrong class")
  if (!inherits(y, c("SpatialPolygons", "SpatialPolygonsDataFrame", "Extent")))
    stop("argument 'y' is the wrong class")

  if (inherits(y, "Extent")) {
    crds <- cbind(c(y[1:2], y[2:1], y[1]), c(rep(y[3], 2), rep(y[4], 2), y[3]))
    y <- SpatialPolygons(list(Polygons(list(Polygon(crds)), "bbox")),
                         proj4string=crs(x))
  }

  if (inherits(x, "SpatialPolygonsDataFrame")) {
    d <- x@data
    rownames(d) <- sapply(slot(x, "polygons"), function(i) slot(i, "ID"))
  } else {
    d <- NULL
  }

  x <- as(x, "SpatialPolygons")
  y <- as(y, "SpatialPolygons")
  y <- y[which(apply(gIntersects(y, x, byid=TRUE), 2, any)), ]

  are.intersecting <- gIntersects(x, y, byid=TRUE)

  FUN <- function (i) {
    if (any(are.intersecting[, i])) {
      y.intersect <- y[are.intersecting[, i]]
      if (is.numeric(buffer.width))
        y.intersect <- gBuffer(y.intersect, width=buffer.width)

      x.geo <- do.call(cmd, list(x[i], gUnaryUnion(y.intersect), byid=TRUE))
      if (inherits(x.geo, "SpatialCollections"))
        x.geo <- gUnaryUnion(x.geo@polyobj)

      is.valid <- suppressWarnings(gIsValid(x.geo, byid=TRUE))
      if (length(is.valid) == 0) return(NULL)
      if (!is.valid) {
        x.geo <- gBuffer(x.geo, width=0)
        ans <- gIsValid(x.geo, byid=TRUE, reason=TRUE)
        if (ans != "Valid Geometry") stop(paste("non-valid polygons:", ans))
      }

      p <- x.geo@polygons[[1]]
      slot(p, "ID") <- slot(x[i]@polygons[[1]], "ID")
    } else {
      p <- if (cmd == "gIntersection") NULL else x[i]@polygons[[1]]
    }
    return(p)
  }
  z <- lapply(seq_along(x), FUN)

  is.retained <- !vapply(z, is.null, TRUE)
  z <- SpatialPolygons(z[is.retained], proj4string=crs(x))
  if (inherits(d, "data.frame")) {
    d <- d[is.retained, , drop=FALSE]
    z <- SpatialPolygonsDataFrame(z, d, match.ID=TRUE)
  }
  return(z)
}
