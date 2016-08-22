AddInsetMap <- function(p, col=c("#D8D8D8", "#BFA76F"),
                        main.label=list(label=NA, adj=NULL),
                        sub.label=list(label=NA, adj=NULL),
                        loc=c("bottomleft", "topleft", "topright", "bottomright"),
                        inset=0.02, width=NULL) {

  op <- par(no.readonly=TRUE)
  on.exit(par(op))

  loc <- match.arg(loc)

  if (!inherits(p, c("SpatialPolygons", "SpatialPolygonsDataFrame")))
    stop("polygon 'p' is the incorrect class")

  usr <- par("usr")
  crds <- cbind(c(usr[1:2], usr[2:1], usr[1]),
                c(rep(usr[3], 2), rep(usr[4], 2), usr[3]))
  b <- SpatialPolygons(list(Polygons(list(Polygon(crds)), "bbox")),
                       proj4string=crs(p))

  if (length(gIntersection(p, b)) == 0)
    stop("user coordinates of the plotting region do not intersect polygon")

  ext <- extent(gUnion(p, b))

  if (is.null(width)) {
    dx  <- 0.2 * diff(usr[1:2])
  } else {
    dx <- width * (diff(usr[1:2]) / par("pin")[1])
  }
  dy <- dx * (diff(ext[3:4]) / diff(ext[1:2]))

  padx <- inset * diff(usr[1:2])
  pady <- inset * diff(usr[3:4])

  if (loc == "bottomleft") {
    loc <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "topleft") {
    loc <- c(usr[1] + padx, usr[4] - pady - dy)
  } else if (loc == "topright") {
    loc <- c(usr[2] - padx - dx, usr[4] - pady - dy)
  } else if (loc == "bottomright") {
    loc <- c(usr[2] - padx - dx, usr[3] + pady)
  }

  rect(loc[1], loc[2], loc[1] + dx, loc[2] + dy, col="#FFFFFFE7", border=NA)

  plt <- c(grconvertX(c(loc[1], loc[1] + dx), "user", "nfc"),
           grconvertY(c(loc[2], loc[2] + dy), "user", "nfc"))
  par(plt=plt, bg="#FFFFFFCC", new=TRUE)

  xlim <- range(ext[1:2])
  ylim <- range(ext[3:4])
  plot.window(xlim=xlim, ylim=ylim)

  plot(p, col=col[1], border=NA,        lwd=0.25, add=TRUE)
  plot(b, col=col[2], border="#090909", lwd=0.25, add=TRUE)
  plot(p, col=NA,     border="#090909", lwd=0.25, add=TRUE)

  if (!is.na(main.label[[1]])) {
    x <- coordinates(gUnaryUnion(p))[1, ]
    text(x[1], x[2], cex=0.7, main.label[[1]], adj=main.label$adj, font=2)
  }
  if (!is.na(sub.label[[1]])) {
    x <- coordinates(gUnaryUnion(b))[1, ]
    text(x[1], x[2], cex=0.6, sub.label[[1]], adj=sub.label$adj)
  }

  box(lwd=0.5)

  invisible(NULL)
}
