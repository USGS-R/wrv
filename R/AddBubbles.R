AddBubbles <- function(x, y=NULL, z, zlim=NULL, inches=c(0, 0.2),
                       scaling=c("perceptual", "mathematical"),
                       bg.pos="red", bg.neg="blue", fg=NA, lwd=0.25,
                       cex=0.7, format=NULL, draw.legend=TRUE,
                       loc=c("bottomleft", "topleft", "topright", "bottomright"),
                       inset=0.02, breaks=NULL, break.labels=NULL,
                       quantile.breaks=FALSE, make.intervals=FALSE,
                       title=NULL, subtitle=NULL,
                       add=TRUE) {

  xy <- xy.coords(x, y,
                  xlab=deparse(substitute(x)),
                  ylab=deparse(substitute(y)))
  x <- xy$x
  y <- xy$y

  if (is.numeric(zlim)) {
    if (is.na(zlim[1])) zlim[1] <- min(z, na.rm=TRUE)
    if (is.na(zlim[2])) zlim[2] <- max(z, na.rm=TRUE)
    z[z < zlim[1] | z > zlim[2]] <- NA
  }
  is <- !is.na(z)
  x <- x[is]
  y <- y[is]
  z <- z[is]

  if (is.null(breaks)) breaks <- pretty(z, n=6)

  if (quantile.breaks) {
    breaks <- quantile(z, probs=seq(0, 1, 0.25))
    if (is.null(break.labels)) {
      val <- formatC(breaks, format=format, big.mark=",")
      lab <- c("minimum", "25th quartile", "median",
               "75th quartile", "maximum")
      break.labels <- sprintf("%s (%s)", val, lab)
    }
  } else if (make.intervals) {
    interval <- findInterval(z, breaks, rightmost.closed=TRUE)
    s <- formatC(breaks, format=NULL, big.mark=",")
    ss <- sprintf("[%s, %s)", head(s, -1), tail(s, -1))
    if (any(interval == 0)) ss[1] <- sprintf("(-Inf, %s)", s[1])
    n <- length(breaks)
    if (any(interval == n)) {
      if (any(z == max(breaks)))
        ss[length(ss)] <- sub(")$", "]", ss[length(ss)])
      else
        ss[length(ss)] <- sprintf("[%s, Inf)", s[length(s) - 1L])
    }
    if (is.null(break.labels)) break.labels <- ss
    idxs <- findInterval(z, breaks, all.inside=TRUE)
    breaks <- (head(breaks, -1) + tail(breaks, -1)) / 2
    z <- breaks[idxs]
  }

  if (is.null(break.labels))
    break.labels <- formatC(breaks, format=format, big.mark=",")

  if (!add) plot(NA, type="n", xlim=extendrange(x), ylim=extendrange(y),
                 xlab="x", ylab="y")

  if (is.na(inches[1])) inches[1] <- 0
  if (is.na(inches[2])) inches[2] <- 0.2
  min.r <- diff(grconvertX(c(0, inches[1]), from="inches", to="user"))
  max.r <- diff(grconvertX(c(0, inches[2]), from="inches", to="user")) - min.r

  scaling <- match.arg(scaling)
  if (scaling == "mathematical")
    Scale <- function(v, max.v, max.r) {return(sqrt(v / max.v) * max.r)}
  else
    Scale <- function(v, max.v, max.r) {return(((v / max.v)^0.57) * max.r)}
  r <- Scale(abs(z), max(abs(c(z, breaks))), max.r) + min.r

  cols <- rep("#02080D", length(z))
  if (is.function(bg.neg))
    cols[z < 0] <- .Map2Color(abs(z[z < 0]), bg.neg)
  else
    cols[z < 0] <- bg.neg
  if (is.function(bg.pos))
    cols[z > 0] <- .Map2Color(z[z > 0], bg.pos)
  else
    cols[z > 0] <- bg.pos

  idxs <- order(r, decreasing=TRUE)
  fg.cols <- if (is.null(fg)) cols[idxs] else fg
  symbols(x[idxs], y[idxs], circles=r[idxs], bg=cols[idxs], fg=fg.cols,
          inches=FALSE, add=TRUE, lwd=lwd)

  if (draw.legend) {
    ipad <- strwidth("0", cex=cex)  # arbitrary choice for inner padding

    lab.width <- max(strwidth(break.labels, cex=cex))

    usr <- par("usr")
    padx <- inset * diff(usr[1:2])
    pady <- inset * diff(usr[3:4])

    r <- Scale(abs(breaks), max(abs(c(z, breaks))), max.r) + min.r

    gap <- max(c(median(r), strheight("O", cex=cex) * 1.5))

    s <- vapply(seq_along(r), function(i) sum(r[1:i]), 0)
    s <- ipad + gap + 2 * s - r + (seq_along(r) - 1L) * gap

    dx <- ipad + max(r) * 2 + ipad + lab.width + ipad
    dy <- max(s) + r[length(r)] + ipad * 2

    if (!is.null(title)) {
      title.width <- strwidth(title, cex=cex, font=2) + ipad * 2
      if (dx < title.width) dx <- title.width
      title.height <- strheight(title, cex=cex, font=2)
      dy <- dy + title.height + ipad
    }

    if (!is.null(subtitle)) {
      subtitle.width <- strwidth(subtitle, cex=cex) + ipad * 2
      if (dx < subtitle.width) dx <- subtitle.width
      subtitle.height <- strheight(subtitle, cex=cex) * 1.5
      dy <- dy + subtitle.height
    }

    loc <- match.arg(loc)
    if (loc == "bottomleft") {
      loc <- c(usr[1] + padx, usr[3] + pady)
    } else if (loc == "topleft") {
      loc <- c(usr[1] + padx, usr[4] - pady - dy)
    } else if (loc == "topright") {
      loc <- c(usr[2] - padx - dx, usr[4] - pady - dy)
    } else if (loc == "bottomright") {
      loc <- c(usr[2] - padx - dx, usr[3] + pady)
    }

    rect(loc[1], loc[2], loc[1] + dx, loc[2] + dy,
         col="#FFFFFFE7", border="black", lwd=0.5)

    x <- rep(loc[1] + ipad + max(r), length(r))
    y <- loc[2] + s

    cols <- rep("#02080D", length(breaks))
    if (is.function(bg.neg))
      cols[breaks < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg)
    else
      cols[breaks < 0] <- bg.neg
    if (is.function(bg.pos))
      cols[breaks > 0] <- .Map2Color(breaks[breaks > 0], bg.pos)
    else
      cols[breaks > 0] <- bg.pos
    fg.cols <- if (is.null(fg)) cols else fg
    symbols(x, y, circles=r, bg=cols, fg=fg.cols, inches=FALSE, add=TRUE,
            lwd=lwd)

    text(loc[1] + ipad + max(r) * 2 + ipad, y, break.labels,
         adj=c(0, 0.5), cex=cex)

    if (!is.null(title))
      text(loc[1] + dx / 2, loc[2] + dy - ipad, title,
           adj=c(0.5, 1), cex=cex, font=2)

    if (!is.null(subtitle))
      text(loc[1] + dx / 2, loc[2] + dy - subtitle.height - title.height - ipad,
           subtitle, adj=c(0.5, 0), cex=cex)
  }

  invisible(NULL)
}

##

.Map2Color <- function(x, Pal, xlim=NULL, n=100L){
  if (length(x) == 0) return(NULL)
  if (is.null(xlim)) xlim <- range(x)
  Pal(n)[findInterval(x, seq(xlim[1], xlim[2], length.out=n), all.inside=TRUE)]
}
