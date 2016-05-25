ExtractAlongTransect <- function(transect, r) {

  if (!inherits(transect, c("SpatialPoints", "SpatialLines")))
    stop("incorrect class for 'transect' argument")
  if (!inherits(r, c("RasterLayer", "RasterStack", "RasterBrick")))
    stop("incorrect class for 'r' argument")

  if (inherits(transect, "SpatialLines"))
    transect <- as(transect, "SpatialPoints")

  crs <- CRS(proj4string(r))

  v <- coordinates(spTransform(transect, crs))
  if (length(v) < 2) stop("number of vertices in transect is < 2")

  r.xmin <- xmin(r)
  r.xmax <- xmax(r)
  r.ymin <- ymin(r)
  r.ymax <- ymax(r)

  rx <- seq(r.xmin, r.xmax, by=xres(r))
  ry <- seq(r.ymin, r.ymax, by=yres(r))

  dist.along.transect <- as.matrix(dist(v))
  segs <- list()
  v.d <- 0

  for (i in seq_len(nrow(v) - 1L)) {

    v.x <- v[i:(i + 1L), 1]
    v.y <- v[i:(i + 1L), 2]

    m <- (v.y[2] - v.y[1]) / (v.x[2] - v.x[1])
    rx.x <- rx[rx > min(v.x) & rx < max(v.x)]
    ry.y <- ry[ry > min(v.y) & ry < max(v.y)]
    rx.y <- m * (rx.x - v.x[1]) + v.y[1]
    ry.x <- (ry.y - v.y[1]) / m + v.x[1]

    x <- c(v.x[1], rx.x, ry.x, v.x[2])
    y <- c(v.y[1], rx.y, ry.y, v.y[2])

    d <- as.matrix(dist(cbind(x, y), diag=TRUE))[, 1]
    idxs <- order(d)
    x <- x[idxs]
    y <- y[idxs]
    d <- d[idxs]
    idxs <- which(x >= r.xmin & x <= r.xmax & y >= r.ymin & y <= r.ymax)
    x <- x[idxs]
    y <- y[idxs]
    d <- d[idxs] + sum(v.d)
    n <- length(d)
    mid.x <- vapply(seq_len(n - 1L), function(j) mean(c(x[j], x[j + 1L])), 0)
    mid.y <- vapply(seq_len(n - 1L), function(j) mean(c(y[j], y[j + 1L])), 0)

    x <- c(x[1], rep(x[2:(n - 1L)], each=2), x[n])
    y <- c(y[1], rep(y[2:(n - 1L)], each=2), y[n])
    d <- c(d[1], rep(d[2:(n - 1L)], each=2), d[n])

    n <- length(x)
    seg <- cbind(x, y, dist=d, matrix(NA, nrow=n, ncol=nlayers(r),
                 dimnames=list(NULL, names(r))))
    colnames(seg) <- make.names(colnames(seg), unique=TRUE)

    for (j in seq_len(nlayers(r))) {
      z <- extract(r[[j]], SpatialPoints(cbind(mid.x, mid.y)))
      seg[, j + 3L] <- rep(z, each=2)
    }
    rownames(seg) <- NULL

    idxs <- NULL
    for (j in seq_len(n)) {
      is.last <- j == n
      if (is.last || all(is.na(seg[j, ]))) {
        if (is.null(idxs)) next
        if (is.last) idxs <- c(idxs, j)
        nsegs <- length(segs)
        if (nsegs == 0 || max(segs[[nsegs]][, "dist"]) != min(d[idxs]))
          segs[[nsegs + 1L]] <- seg[idxs, ]
        else
          segs[[nsegs]] <- rbind(segs[[nsegs]], seg[idxs, ])
        idxs <- NULL
      } else {
        idxs <- c(idxs, j)
      }
    }
    v.d <- c(v.d, dist.along.transect[i, i + 1L])
  }

  FUN <- function(s) {
    SpatialPointsDataFrame(s[, 1:2], data.frame(s[, -(1:2)], row.names=NULL),
                           proj4string=crs, match.ID=FALSE)
  }
  return(lapply(segs, FUN))
}
