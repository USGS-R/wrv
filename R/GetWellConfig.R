GetWellConfig <- function(rs.model, wells, well.col, rate.col=NULL,
                          lay2.hk.tol=1e-02) {

  wells@data$cell <- over(wells, as(rs.model, "SpatialGrid"))
  is.in.model <- as.logical(!is.na(rs.model[["lay1.top"]])[wells@data$cell])
  wells <- wells[is.in.model, , drop=FALSE]

  d <- wells@data

  cols <- c("lay1.top", "lay1.bot", "lay2.bot", "lay3.bot")
  d <- cbind(d, rowColFromCell(rs.model, d$cell),
             subset(rs.model, cols)[d$cell])

  cols <- c("TopOpen1", "BotOpen1", "TopOpen2", "BotOpen2")
  d[, cols] <- d$lay1.top - d[, cols]  # convert depth to elevation

  d$is.lay1 <- with(d, (TopOpen1 == 0 & BotOpen1 == 0) |
                       (TopOpen1 > lay1.bot) |
                       (TopOpen2 > lay1.bot & !is.na(TopOpen2)))
  d$is.lay2 <- with(d, (TopOpen1 > lay2.bot & BotOpen1 < lay1.bot &
                        !is.na(lay2.bot)) |
                       (TopOpen2 > lay2.bot & BotOpen2 < lay1.bot &
                        !is.na(lay2.bot) & !is.na(TopOpen2)))
  d$is.lay3 <- with(d, (TopOpen1 > lay3.bot & BotOpen1 < lay2.bot &
                        !is.na(lay2.bot) & !is.na(lay3.bot)) |
                       (TopOpen2 > lay3.bot & BotOpen2 < lay2.bot &
                        !is.na(lay2.bot) & !is.na(lay3.bot) & !is.na(TopOpen2)))

  d[with(d, TopOpen1 < lay1.bot &  is.na(lay2.bot) &  is.na(lay3.bot)),
    "is.lay1"] <- TRUE
  d[with(d, TopOpen1 < lay2.bot & !is.na(lay2.bot) &  is.na(lay3.bot)),
    "is.lay2"] <- TRUE
  d[with(d, TopOpen1 < lay3.bot & !is.na(lay2.bot) & !is.na(lay3.bot)),
    "is.lay3"] <- TRUE

  d$nlay <- rowSums(d[, c("is.lay1", "is.lay2", "is.lay3")])

  d$lay1.hk <- rs.model[["lay1.hk"]][d$cell]
  d$lay2.hk <- rs.model[["lay2.hk"]][d$cell]
  d$lay3.hk <- rs.model[["lay3.hk"]][d$cell]

  is.lay2.adj <- with(d, !is.lay1 & is.lay2 & !is.lay3 & lay2.hk < lay2.hk.tol)
  d$is.lay1[is.lay2.adj] <- TRUE
  d$is.lay2[is.lay2.adj] <- FALSE

  FUN <- function(i) {
    layers <- paste0("lay", c(i - 1L, i), ".bot")
    if (i == 1)
      layers[1] <- "lay1.top"
    thk.1 <- apply(d[, c("TopOpen1", layers[1])], 1, min) -
             apply(d[, c("BotOpen1", layers[2])], 1, max)
    thk.2 <- apply(d[, c("TopOpen2", layers[1])], 1, min) -
             apply(d[, c("BotOpen2", layers[2])], 1, max)
    thk.1[is.na(thk.1) | thk.1 < 0] <- 0
    thk.2[is.na(thk.2) | thk.2 < 0] <- 0
    thk <- thk.1 + thk.2
    thk[!d[[paste0("is.lay", i)]]] <- NA
    return(thk)
  }
  d[, paste0("lay", 1:3, ".thk")] <- data.frame(lapply(1:3, FUN))

  dd <- data.frame(rep(d[[well.col]], d$nlay))
  names(dd) <- well.col

  dd$lay <- unlist(apply(d[, c("is.lay1", "is.lay2", "is.lay3")], 1L, which))
  dd <- left_join(dd, d[, c(well.col, "cell", "row", "col")], by=well.col)
  for (i in 1:3) {
    is <- dd$lay == i
    dd$hk[is] <- rs.model[[paste0("lay", i, ".hk")]][dd$cell[is]]
    idxs <- match(dd[[well.col]][is], d[[well.col]])
    dd$thk[is] <- d[[paste0("lay", i, ".thk")]][idxs]
  }

  dd$frac <- NA
  for (i in unique(dd[[well.col]])) {
    idxs <- which(dd[[well.col]] == i)
    if (length(idxs) == 1L) {
      frac <- 1
    } else {
      transmissivity <- dd$hk[idxs] * dd$thk[idxs]
      frac <- transmissivity / sum(transmissivity)
    }
    dd[idxs, "frac"] <- frac
  }

  if (!is.null(rate.col)) {
    dd <- left_join(dd, d[, c(well.col, rate.col), drop=FALSE], by=well.col)
    dd[, rate.col] <- dd[, rate.col] * dd$frac
  }

  dd$cell <- NULL

  invisible(dd)
}
