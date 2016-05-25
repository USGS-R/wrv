AddScaleBar <- function(asp=1, unit=NULL, is.lonlat=FALSE,
                        loc=c("bottomleft", "topleft", "topright", "bottomright"),
                        offset=c(0, 0), lab.vert.exag=NULL) {
  loc <- match.arg(loc)
  usr <- par("usr")
  pin <- par("pin")

  if (is.lonlat) {
    y <- (usr[3] + usr[4]) / 2
    xaxp <- par("xaxp")
    dx1 <- diff(xaxp[1:2]) / xaxp[3]
    dm1 <- spDistsN1(cbind(0, y), c(dx1, y), longlat=TRUE)
    dm2 <- diff(pretty(c(0, dm1), 1)[1:2])
    d <- (dx1 * dm2) / dm1
    label <- paste(format(dm2), "km")
  } else {
    d <- diff(pretty(usr[1:2]))[1]
    label <- format(d, big.mark=",")
    if (!is.null(unit)) label <- paste(label, unit)
  }

  padx <- 0.05 * (usr[2] - usr[1])
  pady <- 0.05 * (usr[2] - usr[1]) / asp
  if (loc == "bottomleft") {
    loc <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "topleft") {
    loc <- c(usr[1] + padx, usr[4] - pady)
  } else if (loc == "topright") {
    loc <- c(usr[2] - padx - d, usr[4] - pady)
  } else if (loc == "bottomright") {
    loc <- c(usr[2] - padx - d, usr[3] + pady)
  }
  loc[1] <- loc[1] + offset[1] * (diff(usr[1:2]) / pin[1])
  loc[2] <- loc[2] + offset[2] * (diff(usr[3:4]) / pin[2])

  divs <- 1L
  for (i in 5:3) {
    if (d %% i == 0) {
      divs <- i
      break
    }
  }
  xat <- seq(loc[1], loc[1] + d, length.out=(divs + 1L))
  tcl <- (diff(usr[1:2]) * 0.01) / asp

  lines(rbind(c(loc[1], loc[2]), c(loc[1] + d, loc[2])), lwd=0.5)
  for (i in xat) lines(rbind(c(i, loc[2]), c(i, loc[2] + tcl)), lwd=0.5)
  text(loc[1], loc[2] + tcl, "0", adj=c(0.5, -0.5), cex=0.7)
  text(loc[1] + d, loc[2] + tcl, label, adj=c(0.3, -0.5), cex=0.7)

  if (is.logical(lab.vert.exag)) {
    add.label <- lab.vert.exag
  } else {
    add.label <- if (asp > 20) TRUE else FALSE
  }
  if (add.label) {
    txt <- sprintf("VERTICAL EXAGGERATION x%s", asp)
    text(loc[1] + d / 2, loc[2], txt, cex=0.7, pos=1)
  }
}
