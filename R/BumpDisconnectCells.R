BumpDisconnectCells <- function(rs, min.overlap=2, bump.by=0.1, max.itr=1e+04) {
  r <- rs[[2]]

  cell <- which(!is.na(r[]))
  rows <- rowFromCell(r, cell)
  cols <- colFromCell(r, cell)

  d <- cbind(cell, c1=NA, c2=NA, c3=NA, c4=NA)
  d[, "c1"] <- cellFromRowCol(r, rows + 1L, cols)
  d[, "c2"] <- cellFromRowCol(r, rows, cols - 1L)
  d[, "c3"] <- cellFromRowCol(r, rows - 1L, cols)
  d[, "c4"] <- cellFromRowCol(r, rows, cols + 1L)

  itr <- 0L
  while(TRUE) {

    FUN <- function(i) {
      x <- rep(NA, nrow(d))
      is <- !is.na(d[, i])
      x[is] <- r[d[is, "cell"]] >= rs[[1]][d[is, i]] - min.overlap
      return(x)
    }
    m <- cbind(FUN("c1"), FUN("c2"), FUN("c3"), FUN("c4"))
    is.disconnected <- apply(m, 1, any, na.rm=TRUE)
    if (all(!is.disconnected)) break

    cells.to.bump <- d[is.disconnected, "cell"]
    r[cells.to.bump] <- r[cells.to.bump] - bump.by

    itr <- itr + 1L
    if (itr > max.itr) {
      warning("maximum iterations reached")
      break
    }

    d <- d[is.disconnected, , drop=FALSE]
  }
  return(r - rs[[2]])
}
