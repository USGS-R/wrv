PlotGraph <- function(x, y, xlab, ylab, asp=NA, xlim=NULL, ylim=NULL,
                      xn=5L, yn=5L, ylog=FALSE, type="s", lty=1, lwd=1,
                      pch=NULL, col=NULL, bg=NA, fill=NULL, pt.cex=1,
                      seq.date.by="year", scientific=NA,
                      conversion.factor=NULL, boxwex=0.8,
                      center.date.labels=FALSE, bg.polygon=NULL) {

  scientific <- as.logical(scientific)
  scientific <- rep(scientific, length.out=3)

  if (inherits(x, c("data.frame", "matrix"))) {
    if (inherits(x, "tbl_df")) x <- as.data.frame(x)
    if (missing(y)) y <- x[, -1, drop=FALSE]
    x <- x[, 1]
  }
  y <- as.matrix(y)

  if (inherits(x, "Date")) {
    if (!inherits(xlim, "Date")) xlim <- range(x)
    xat <- seq(xlim[1], xlim[2], seq.date.by)

  } else if (inherits(x, c("character", "factor"))) {
    x <- seq_along(x)
    xat <- x
    xlim <- extendrange(x)

  } else {
    if (is.numeric(xlim)) {
      xat <- pretty(xlim, n=xn)
    } else {
      xat <- pretty(range(x, na.rm=TRUE), n=xn)
      xlim <- range(xat)
    }
  }
  if (is.numeric(ylim)) {
    if (ylog)
      yat <- axisTicks(log10(ylim), TRUE, nint=yn)
    else
      yat <- pretty(ylim, n=yn)
  } else {
    if (ylog)
      yat <- axisTicks(log10(range(y)), TRUE, nint=yn)
    else
      yat <- pretty(range(y, na.rm=TRUE), n=yn)
    ylim <- range(yat)
  }

  n <- ncol(y)
  if (!is.character(col) && !is.logical(col))
    col <- if (is.function(col)) col(n) else rainbow(n, start=0.0, end=0.8)
  lty <- rep_len(lty, length.out=n)
  lwd <- rep_len(lwd, length.out=n)

  mar <- c(2.3, 4.1, 0.5, 4.1)
  if (is.null(conversion.factor)) mar[4] <- 2.1
  mgp <- c(3.2, 0.2, 0)  # cumulative axis margin line: title, labels, and line
  op <- par(mar=mar, mgp=mgp)
  line.in.inches <- (par("mai") / par("mar"))[2]

  plot.new()
  plot.window(xlim=xlim, ylim=ylim, asp=asp, xaxt="n", yaxt="n",
              xaxs="i", yaxs="i", log=ifelse(ylog, "y", ""))

  cex <- 0.7
  tcl <- 7.2 / par("cra")[2]
  is.decreasing <- diff(par("usr")[1:2]) < 0

  if (is.list(bg.polygon)) {
    bg.col <- if (is.null(bg.polygon$col)) NA else bg.polygon$col
    polygon(bg.polygon$x, col=bg.col, border=NA)
  }

  abline(v=xat, col="lightgrey", lwd=0.5)
  abline(h=yat, col="lightgrey", lwd=0.5)

  if (type %in% c("l", "s") & is.character(fill)) {
    for (i in seq_len(ncol(y))) {
      if (is.na(fill[i])) next
      xx <- as.numeric(x)
      yy <- as.numeric(y[, i])
      grp <- .GetSegmentGroup(yy)
      for (j in unique(na.omit(grp))) {
        idxs <- which(grp %in% j)
        xxx <- xx[idxs]
        yyy <- yy[idxs]
        if (type == "s") {
          xxx <- sort(c(xxx, tail(xxx, -1)), decreasing=is.decreasing)
          yyy <- head(rep(yyy, each=2), -1)
          max.idx <- max(idxs)
          if (max.idx < length(xx)) {
            xxx <- c(xxx, xx[max.idx + 1L])
            yyy <- c(yyy, tail(yyy, 1))
          }
        }
        xxx <- c(xxx, tail(xxx, 1), xxx[1])
        ylim <- sort(par("usr")[3:4])
        ymin <- if (ylim[1] < 0 & ylim[2] > 0) 0 else ylim[which.min(abs(ylim))]
        yyy <- c(yyy, rep(ymin, 2))
        polygon(xxx, yyy, col=fill[i], border=NA)
      }
    }
  }

  if (inherits(x, "Date")) {
    if (center.date.labels) {

      if (tail(xat, 1) < xlim[2])
        at <- xat + diff(c(xat, xlim[2])) / 2
      else
        at <- head(xat, -1) + diff(xat) / 2

      axis.Date(1, at=xat, tcl=tcl, labels=FALSE, lwd=-1, lwd.ticks=0.5)
      axis.Date(1, at=at, tcl=0, cex.axis=cex, lwd=-1)
    } else {
      axis.Date(1, at=xat, tcl=tcl, cex.axis=cex, lwd=-1, lwd.ticks=0.5)
    }
    axis.Date(3, at=xat, tcl=tcl, labels=FALSE, lwd=-1, lwd.ticks=0.5)
  } else {
    if (is.na(scientific[1])) {
      xlabels <- formatC(xat, big.mark=",")
    } else if (scientific[1]) {
      digits <- format.info(as.numeric(xat))[2]
      while (TRUE) {
        xlabels <- ToScientific(xat, digits=digits, lab.type="plotmath")
        if (!any(duplicated(unlist(lapply(xlabels, eval)))) | digits > 2) break
        digits <- digits + 1L
      }
    } else {
      s <- format(xat, big.mark=",", scientific=FALSE)
      xlabels <- sub("^\\s+", "", s)
    }
    axis(1, at=xat, labels=xlabels, tcl=tcl, las=1, cex.axis=cex, lwd=-1,
         lwd.ticks=0.5)
    axis(3, at=xat, tcl=tcl, lwd=-1, lwd.ticks=0.5, labels=FALSE)
  }
  if (is.na(scientific[2])) {
    ylabels <- formatC(yat, big.mark=",")
  } else if (scientific[2]) {
      digits <- format.info(as.numeric(yat))[2]
      while (TRUE) {
        ylabels <- ToScientific(yat, digits=digits, lab.type="plotmath")
        if (!any(duplicated(unlist(lapply(ylabels, eval)))) | digits > 2) break
        digits <- digits + 1L
      }
  } else {
    s <- format(yat, big.mark=",", scientific=FALSE)
    ylabels <- sub("^\\s+", "", s)
  }
  axis(2, at=yat, labels=ylabels, tcl=tcl, las=1, cex.axis=cex, lwd=-1,
       lwd.ticks=0.5)
  if (!missing(xlab)) {
    mar.line <- sum(par("mgp")[2:3]) + par("mgp")[2] + cex
    title(xlab=xlab, cex.lab=cex, line=mar.line)
  }
  if (!missing(ylab)) {
    max.sw <- max(strwidth(ylabels, units="inches")) * cex
    mar.line <- max.sw / line.in.inches + sum(par("mgp")[2:3]) + par("mgp")[2]
    title(ylab=ylab[1], cex.lab=cex, line=mar.line)
  }

  if (is.null(conversion.factor)) {
    axis(4, at=yat, tcl=tcl, lwd=-1, lwd.ticks=0.5, labels=FALSE)
  } else {

    if (ylog)
      yat <- axisTicks(log10(range(yat * conversion.factor)), TRUE, nint=yn)
    else
      yat <-  pretty(yat * conversion.factor, n=yn)

    if (is.na(scientific[3])) {
      ylabels <- formatC(yat, big.mark=",")
    } else if (scientific[3]) {
      digits <- format.info(as.numeric(yat))[2]
      while (TRUE) {
        ylabels <- ToScientific(yat, digits=digits, lab.type="plotmath")
        if (!any(duplicated(unlist(lapply(ylabels, eval)))) | digits > 2) break
        digits <- digits + 1L
      }
    } else {
      s <- format(yat, big.mark=",", scientific=FALSE)
      ylabels <- sub("^\\s+", "", s)
    }
    axis(4, at=(yat / conversion.factor), labels=ylabels, tcl=tcl, las=1,
         cex.axis=cex, lwd=-1, lwd.ticks=0.5)
    if (!missing(ylab) && length(ylab) > 1) {
      max.sw <- max(strwidth(ylabels, units="inches")) * cex
      mar.line <- max.sw / line.in.inches + sum(par("mgp")[2:3])
      mtext(ylab[2], side=4, cex=cex, line=mar.line)
    }
  }

  box(lwd=0.5)

  x <- as.numeric(x)
  is.x <- x >= as.numeric(xlim[1]) & x <= as.numeric(xlim[2])
  x <- x[is.x]
  y <- y[is.x, , drop=FALSE]
  y[y < ylim[1] & y > ylim[2]] <- NA

  if (type == "box") {
    boxplot(y, xaxt="n", yaxt="n", range=0, varwidth=TRUE, boxwex=boxwex,
            col=col, border="#333333", add=TRUE, at=x)
  } else {
    if (type == "s") {
      for (i in seq_len(ncol(y))) {
        xx <- as.numeric(x)
        yy <- as.numeric(y[, i])
        grp <- .GetSegmentGroup(yy)
        for (j in unique(na.omit(grp))) {
          idxs <- which(grp %in% j)
          xxx <- sort(c(xx[idxs], tail(xx[idxs], -1)), decreasing=is.decreasing)
          yyy <- head(rep(yy[idxs], each=2), -1)
          max.idx <- max(idxs)
          if (max.idx < length(xx)) {
            xxx <- c(xxx, xx[max.idx + 1L])
            yyy <- c(yyy, tail(yyy, 1))
          }
          lines(xxx, yyy, lty=lty[i], lwd=lwd[i], col=col[i])
        }
      }
    } else {
      matplot(x, y, xaxt="n", yaxt="n", type=type, lty=lty, lwd=lwd, pch=pch,
              col=col, bg=bg, cex=pt.cex, add=TRUE, verbose=FALSE)
    }
  }

  invisible(NULL)
}


.GetSegmentGroup <- function (y) {
  grp <- as.integer(!is.na(y))
  mult <- 1L
  for (i in seq_along(grp)[-1]) {
    if (grp[i - 1L] > 0L & grp[i] == 0L) mult <- mult + 1L
    grp[i] <- grp[i] * mult
  }
  grp[grp == 0L] <- NA
  return(grp)
}
