PlotCrossSection <- function(transect, rs, geo.lays=names(rs), val.lays=NULL,
                             wt.lay=NULL, asp=1, ylim=NULL,
                             max.dev.dim=c(43, 56), n=NULL, breaks=NULL,
                             pal=NULL, col=NULL, ylab=NULL, unit=NULL,
                             id=c("A", "A'"), labels=NULL, explanation=NULL,
                             features=NULL, max.feature.dist=Inf, draw.key=TRUE,
                             draw.sep=TRUE, is.categorical=FALSE,
                             contour.lines=NULL, bg.col="#E1E1E1",
                             wt.col="#FFFFFFD8") {

  if (!inherits(transect, "SpatialLines"))
    stop("incorrect class for 'transect' argument")
  if (!inherits(rs, c("RasterStack", "RasterBrick")))
    stop("incorrect class for 'rs' argument")
  if (!all(c(geo.lays, val.lays) %in% names(rs)))
    stop("layer names not found in raster stack")
  if (length(val.lays) >= length(geo.lays))
    stop("number of value layers is greater than the number of geometry layers")

  transect <- spTransform(transect, crs(rs))
  rs <- crop(rs, extent(as.vector(t(bbox(transect)))), snap="out")
  layer.names <- unique(c(geo.lays, val.lays, wt.lay))
  eat <- ExtractAlongTransect(transect, subset(rs, layer.names))

  cell.values <- NULL
  cell.polys  <- list()
  for (i in seq_along(val.lays)) {
    for (j in seq_along(eat)) {
      seg <- as.matrix(eat[[j]]@data)
      for (k in seq(1, nrow(seg) - 1L, by=2)) {
        v <- as.numeric(seg[k, val.lays[i]])
        p <- rbind(seg[c(k, k + 1L), c("dist", geo.lays[i])],
                   seg[c(k + 1L, k), c("dist", geo.lays[i + 1L])],
                   seg[k, c("dist", geo.lays[i]), drop=FALSE])
        if (anyNA(p)) next
        cell.values <- c(cell.values, v)
        cell.polys  <- c(cell.polys, Polygon(p))
      }
    }
  }

  at <- NULL
  if (!is.null(cell.values)) {
    if (!is.function(pal)) {
      if (requireNamespace("colorspace", quietly=TRUE)) {
        pal <- function(n) {
          colorspace::rainbow_hcl(n, start=0.0, end=(360 * (n - 1) / n) * 0.8)
        }
      } else {
        pal <- function(n) rainbow(n, start=0.0, end=0.8)
      }
    }
    if (is.categorical) {
      unique.vals <- sort(unique(cell.values))
      at <- seq_along(unique.vals)
      if (!is.character(col)) col <- pal(length(at))
      cell.cols <- col[match(cell.values, unique.vals)]
    } else {
      if (!is.numeric(n)) n <- 200L
      if (!is.numeric(breaks)) {
        at <- pretty(cell.values, n=8)
        breaks <- seq(min(at), max(at), length.out=n)
      }
      intervals <- findInterval(cell.values, breaks, all.inside=TRUE)
      if (!is.character(col)) col <- pal(length(breaks) - 1L)
      cell.cols <- col[intervals]
    }
    cols <- unique(cell.cols)

    FUN <- function(i) {
      p <- Polygons(cell.polys[which(cell.cols == cols[i])], i)
      p <- gUnaryUnion(SpatialPolygons(list(p), i))
      p <- slot(p, "polygons")[[1]]
      p@ID <- cols[i]
      return(p)
    }
    p <- lapply(seq_along(cols), FUN)
    cell.polys.merged <- SpatialPolygons(p, seq_along(cols))
  }

  x <- unlist(lapply(eat, function(i) i@data$dist))
  xlim <- range(x, na.rm=TRUE)
  xat <- pretty(xlim)

  y <- unlist(lapply(eat, function(i) unlist(i@data[, geo.lays])))
  if (is.numeric(ylim)) {
    yat <- pretty(ylim)
  } else {
    yat <- pretty(range(y, na.rm=TRUE))
    ylim <- range(yat, na.rm=TRUE)
  }

  inches.in.pica <- 1 / 6
  mar2 <- c(1, 4.6, 4, 2)
  if (draw.key) {
    y1 <- 1
    mar1 <- c(2, mar2[2], 1, mar2[4])
  } else {
    y1 <- 0
    mar1 <- c(0, 0, 0, 0)
  }
  if (dev.cur() > 1) {
    dev.dim <- dev.size() / inches.in.pica
    w <- dev.dim[1]
    h1 <- y1 + mar1[1] + mar1[3]
    h2 <- dev.dim[2] - h1
    h <- h1 + h2
  } else {
    w <- max.dev.dim[1]
    repeat {
      y2 <- (w - mar2[2] - mar2[4]) * (diff(ylim) / diff(xlim)) * asp
      h2 <- y2 + mar2[1] + mar2[3]
      h1 <- y1 + mar1[1] + mar1[3]
      h <- h1 + h2
      if (h > max.dev.dim[2]) w <- w - 0.01 else break
    }
    wi <- w * inches.in.pica
    hi <- h * inches.in.pica
    dev.new(width=wi, height=hi)
  }

  if (draw.key) {
    layout(matrix(c(2, 1), nrow=2, ncol=1), heights=c(h2, h1) / h)
    if (!is.null(labels$at)) at <- labels$at
    labs <- if (is.null(labels$labels)) TRUE else labels$labels
    AddColorKey(mai=mar1 * inches.in.pica, is.categorical=is.categorical,
                breaks=breaks, col=col, at=at, labels=labs,
                explanation=explanation)
  } else {
    layout(matrix(1, nrow=1, ncol=1))
  }

  par(mai=mar2 * inches.in.pica, mgp=c(3, 0.6, 0))

  plot(NA, type="n", xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", bty="n",
       xaxt="n", yaxt="n", xlab="", ylab="", asp=asp)

  lwd <- 0.5
  cex <- 0.7
  tcl <- -7.2 / par("cra")[2]
  usr <- par("usr")

  if (is.character(bg.col)) {
    FUN <- function(i) {
      m <- cbind(x=i@data[[1]], y=i@data[[2]])
      m <- rbind(m, cbind(rev(range(m[, "x"], na.rm=TRUE)), usr[3]),
                 m[1, , drop=FALSE])
      return(Polygon(m))
    }
    bg.poly <- SpatialPolygons(list(Polygons(lapply(eat, FUN), "bg")), 1L)
    plot(bg.poly, col=bg.col, border=NA, lwd=0.1, add=TRUE)
  }

  plot(cell.polys.merged, col=cols, border=cols, lwd=0.1, add=TRUE)

  if (!is.null(wt.lay) && wt.lay[1] %in% names(rs)) {
    for (s in eat)
      lines(s@data[["dist"]], s@data[, wt.lay[1]], lwd=lwd, col=wt.col)
  }

  lays <- if (draw.sep) geo.lays else c(head(geo.lays, 1), tail(geo.lays, 1))
  for (s in eat)
    matplot(s@data[["dist"]], s@data[, lays], xaxt="n", yaxt="n",
            type="l", lty=1, lwd=lwd, col="#1F1F1F", add=TRUE)

  if (is.list(contour.lines)) {
    e <- extent(cell.polys.merged)
    nc <- 200
    dx <- diff(e[1:2]) / nc
    nr <- as.integer(diff(e[3:4]) / (dx / asp))
    r <- raster(e, nrows=nr, ncols=nc)
    FUN <- function(i) Polygons(list(cell.polys[[i]]), as.character(i))
    p <- lapply(seq_along(cell.polys), FUN)
    p <- SpatialPolygons(p, seq_along(cell.polys))
    r <- rasterize(p, r)
    r[] <- cell.values[r[]]

    color <- as.character(contour.lines[["col"]])
    drawl <- as.logical(contour.lines[["drawlabels"]])
    metho <- as.character(contour.lines[["method"]])
    color <- ifelse(length(color) == 1 && !is.na(color), color, "#1F1F1F")
    drawl <- ifelse(length(drawl) == 1 && !is.na(drawl), drawl, TRUE)
    metho <- ifelse(length(metho) == 1 && !is.na(metho), metho, "flattest")
    contour.breaks <- if (length(breaks) > 20) pretty(breaks, 20L) else breaks
    ncontours <- length(contour.breaks)
    raster::contour(r, maxpixels=length(r), levels=contour.breaks,
                    labels=formatC(contour.breaks, big.mark=","), xlim=xlim,
                    ylim=ylim, labcex=0.5, drawlabels=drawl, method=metho,
                    axes=FALSE, col=color, lwd=lwd, add=TRUE)
  }

  ylabs <- format(yat, big.mark=",")

  axis(4, at=yat, labels=FALSE, lwd=0, lwd.ticks=lwd, tcl=tcl)
  axis(2, at=yat, labels=ylabs, lwd=0, lwd.ticks=lwd, tcl=tcl,
       cex.axis=cex, las=1)

  if (!is.null(ylab)) {
    line.in.inches <- (par("mai") / par("mar"))[2]
    max.sw <- max(strwidth(ylabs, units="inches")) * cex
    mar.line <- max.sw / line.in.inches + sum(par("mgp")[2:3]) + par("mgp")[2]
    title(ylab=ylab, cex.lab=cex, line=mar.line)
  }

  abline(v=xlim, col="black", lwd=lwd)
  abline(h=usr[3],   col="black", lwd=lwd)

  if (!is.null(unit)) mtext(unit, at=usr[1], cex=cex, line=0.2, adj=1)

  if (is.character(id)) {
    if (length(id) == 1) id <- c(id, paste0(id, "'"))
    mtext(id[1], at=usr[1], cex=cex, line=1, adj=0.5, font=4)
    mtext(id[2], at=usr[2], cex=cex, line=1, adj=0.5, font=4)
  }

  par(xpd=TRUE)
  y <- unlist(lapply(eat, function(i) i@data[[geo.lays[1]]]))
  GetGeoTop <- approxfun(x, y)
  pady <- diff(usr[3:4]) * 0.02
  d <- as.matrix(dist(coordinates(as(transect, "SpatialPoints"))))[, 1]
  dist.to.bend <- head(d[-1], -1)
  for (d in dist.to.bend) {
    y <- GetGeoTop(d)
    lines(c(d, d), c(ylim[1], y + pady), lwd=0.3, col="#999999")
    text(d, y + pady, "BEND", adj=c(-0.1, 0.5), col="#999999", cex=0.6, srt=90)
  }
  if (!is.null(features)) {
    tran.pts <- do.call("rbind", eat)
    for (i in seq_len(length(features))) {
      pnt <- spTransform(features, crs(rs))[i, ]
      dist.to.transect <- gDistance(pnt, tran.pts, byid=TRUE)
      idx <- which.min(dist.to.transect)
      if (dist.to.transect[idx] > max.feature.dist) next
      d <- x[idx]
      y <- GetGeoTop(d)
      lines(c(d, d), c(y, y + pady), lwd=0.3)
      label <- format(pnt@data[1, 1])
      text(d, y + pady, label, adj=c(-0.1, 0.5), cex=cex, srt=90)
    }
  }
  par(xpd=FALSE)

  AddScaleBar(asp, unit, loc="bottomright", offset=c(-0.3, 0))

  invisible(list(din=par("din"), usr=usr, heights=c(h2, h1) / h))
}
