AddColorKey <- function(mai, is.categorical, breaks, col, at=NULL,
                        labels=TRUE, scientific=FALSE, explanation=NULL,
                        padx=0.2) {

  if (!missing(mai)) {
    mai[2] <- mai[2] + padx
    mai[4] <- mai[4] + padx
    op <- par(mai=mai)
    on.exit(par(op))
  }

  if (is.categorical) {
    n <- max(c(if (missing(col)) 0 else length(col), length(labels)))
    at <- seq_len(n)
    if (length(n) == 0)
      stop("categorical data requires colors and (or) labels")
    breaks <- c(0.5, seq_len(n) + 0.5)
  } else if (missing(breaks)) {
    stop("missing breaks argument for continous data")
  }

  if (missing(col)) col <- rainbow(length(breaks) - 1L, start=0.0, end=0.8)

  if (is.null(at)) at <- breaks

  cex <- 0.7
  lwd <- 0.5

  xlim <- range(breaks)
  plot(NA, type="n", xlim=xlim, ylim=c(0, 1), xaxs="i", yaxs="i", bty="n",
       xaxt="n", yaxt="n", xlab="", ylab="")

  if (is.categorical) {
    bw <- 2 / 6
    pin <- par("pin")
    repeat {
      if (bw < pin[1] || bw == 0.1) break
      bw <- bw - 0.1
    }
    dx <- (diff(xlim) / pin[1]) * bw / 2
    x <- seq_along(col)
    rect(xleft=x - dx, ybottom=0, xright=x + dx, ytop=1, col=col, border=NA)
  } else {
    rect(xleft=head(breaks, -1), ybottom=0, xright=tail(breaks, -1), ytop=1,
         col=col, border=col, lwd=lwd)
    if (length(at) >= length(breaks) - 1L) {
      abline(v=breaks, lwd=lwd)
    } else {
      segments(x0=at, y0=0, y1=0.25, lwd=lwd)
      segments(x0=at, y0=1, y1=0.75, lwd=lwd)
    }
    box(lwd=lwd)
  }

  if (is.logical(labels) && labels) {
    labels <- if (is.null(at)) axTicks(1) else at
    if (scientific)
      labels <- ToScientific(labels, lab.type="plotmath")
    else
      labels <- formatC(labels, big.mark=",")
  }

  axis(1, at=at, labels=labels, lwd=-1, lwd.ticks=-1, cex.axis=cex, padj=0,
       mgp=c(3, 0, 0))

  if (!is.null(explanation))
    mtext(explanation, side=3, line=0.6, padj=1, adj=0, cex=cex)
}
