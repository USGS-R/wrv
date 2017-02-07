#' Get Well Completion and Pumping Rate in Model Space
#'
#' This function determines well completions and pumping rates in model space.
#' The pumping rate is specified for each model cell intersecting a well's open interval(s)
#' and calculated by multiplying the estimated pumping demand by the cell's transmissivity fraction.
#' The transmissivity fraction is calculated by dividing the cell's aquifer transmissivity by
#' the sum of all transmissivity values for cells belonging to the same well.
#' The transmissivity fraction calculation assumes saturated conditions in the model cell.
#'
#' @param rs.model RasterStack.
#'   Composed of raster layers describing the model grid and hydraulic conductivity distribution:
#'     \code{lay1.top}, \code{lay1.bot}, \code{lay2.bot}, \code{lay3.bot},
#'     \code{lay1.hk}, \code{lay2.hk}, and \code{lay3.hk}.
#' @param wells SpatialPointsDataFrame.
#'   Average pumping rate for each well during various times.
#' @param well.col character.
#'   Column name of the well identifier field.
#' @param rate.col character.
#'   Vector of column names for the pumping rate fields.
#' @param lay2.hk.tol numeric.
#'   Hydraulic conductivity tolerance for model cells in layer 2.
#'   Used to prevent pumping in the aquitard layer of the aquifer system.
#'   Pumping is prohibited in model layer 2 cells with hydraulic conductivity values less than
#'   \code{lay2.hk.tol} and a well opening isolated to layer 2;
#'   for these cases, pumping is allocated to the adjacent layer 1 cell.
#'
#' @return Returns an object of class data.frame with the following components:
#'   \describe{
#'     \item{\dots}{unique identifier assigned to a well, its name is specified by \code{well.col}.}
#'     \item{lay,row,col}{layer, row, and column number of a model cell, respectively.}
#'     \item{hk}{hydraulic conductivity of the model cell, in meters per day.}
#'     \item{thk}{vertical length of the well opening (open borehole or screen) in the model cell, in meters.
#'        A value of zero indicates that the well opening is unknown or below the modeled bedrock surface.}
#'     \item{frac}{transmissivity fraction for a model cell,
#'       where transmissivity is defined as \code{hk} multiplied by \code{thk}.}
#'     \item{\dots}{pumping rate allocated to the model cell for each time period
#'       specified by \code{rate.col}, in cubic meters per day.
#'       The pumping rate is calculated by multiplying the pumping demand for a well
#'       (specified in \code{wells}) by \code{frac}.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#'   A.H. Wylie, Idaho Department of Water Resources
#'
#' @keywords manip
#'
#' @import sp
#' @import raster
#'
#' @export
#'
#' @examples
#' \dontrun{# see Appendix D. Uncalibrated Groundwater-Flow Model}
#'

GetWellConfig <- function(rs.model, wells, well.col, rate.col=NULL,
                          lay2.hk.tol=1e-02) {

  wells@data$cell <- sp::over(wells, methods::as(rs.model, "SpatialGrid"))
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
  dd <- dplyr::left_join(dd, d[, c(well.col, "cell", "row", "col")], by=well.col)
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
    dd <- dplyr::left_join(dd, d[, c(well.col, rate.col), drop=FALSE], by=well.col)
    dd[, rate.col] <- dd[, rate.col] * dd$frac
  }

  dd$cell <- NULL

  invisible(dd)
}
