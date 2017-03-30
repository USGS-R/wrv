#' Run Water Balance
#'
#' This function estimates natural and incidental groundwater recharge at the water table,
#' and pumping demand at production wells.
#' A water-balance approach is used to calculate these volumetric flow rate estimates,
#' where positive values are flow into the aqufer system (groundwater recharge),
#' and negative values are flow out of the system (groundwater discharge).
#'
#' @param r.grid RasterLayer.
#'   Gridded numeric values where NA indicates an \sQuote{inactive} cell in
#'   the top model layer.
#' @param tr.stress.periods Date.
#'   Vector of start and end dates for each stress period in the simulation.
#' @param ss.stress.periods Date.
#'   Vector of start and end dates for stress periods used to create steady-state conditions.
#' @param canal.seep data.frame.
#'   See \code{\link{canal.seep}} dataset for details.
#' @param comb.sw.irr data.frame.
#'   See \code{\link{comb.sw.irr}} dataset for details.
#' @param div.gw data.frame.
#'   See \code{\link{div.gw}} dataset for details.
#' @param div.sw data.frame.
#'   See \code{\link{div.sw}} dataset for details.
#' @param div.ww data.frame.
#'   See \code{\link{div.gw}} dataset for details.
#' @param efficiency data.frame.
#'   See \code{\link{efficiency}} dataset for details.
#' @param entity.components list.
#'   See \code{\link{entity.components}} dataset for details.
#' @param et RasterStack.
#'   See \code{\link{et}} dataset for details.
#' @param irr.entities SpatialPolygonsDataFrame.
#'   See \code{\link{irr.entities}} dataset for details.
#' @param land.surface RasterLayer.
#'   See \code{\link{land.surface}} dataset for details.
#' @param pod.gw data.frame.
#'   See \code{\link{pod.gw}} dataset for details.
#' @param priority.cuts data.frame.
#'   See \code{\link{priority.cuts}} dataset for details.
#' @param r.canals RasterLayer.
#'   See \code{\link{r.canals}} dataset for details.
#' @param rs.entities RasterStack.
#'   See \code{\link{rs.entities}} dataset for details.
#' @param rs.rech.non.irr RasterStack.
#'   See \code{\link{rs.rech.non.irr}} dataset for details.
#' @param verbose logical.
#'   Indicates whether to return summary tables:
#'     \code{natural.rech}, \code{inciden.rech}, and \code{pumping.rech}.
#    See \sQuote{Value} section for table formats.
#'
#' @return Returns an object of class list with the following components:
#'
#'   (1) Water-table flow data (combines natural and incidental groundwater recharge)
#'       are stored in \code{areal.rech},
#'       an object of RasterStack class with raster layers for each model stress period;
#'       cell values are specified as volumetric flow rates in cubic meters per day.
#'
#'   (2) Production well pumping data are stored in \code{pod.rech},
#'       an object of \code{data.frame} class with the following components:
#'       \describe{
#'         \item{WMISNumber}{unique number assigned to a water right point of diversion.}
#'         \item{ss,199501,\dots,201012}{volumetric flow rate, specified for each stress period,
#'           in cubic meters per day.}
#'       }
#'
#'   (3) Natural groundwater recharge data are stored in \code{natural.rech},
#'       an object of data.frame class with the following components:
#'       \describe{
#'         \item{YearMonth}{calendar year and month YYYYMM.}
#'         \item{Area}{land-surface area of non-irrigated lands, in square meters.}
#'         \item{ET}{evapotranspiration on non-irrigated lands, in cubic meters per month.}
#'         \item{Rech}{volumetric flow rate, in cubic meters per month.}
#'       }
#'
#'   (4) Incidental groundwater recharge data are stored in \code{inciden.rech},
#'       an object of data.frame class with the following components:
#'       \describe{
#'         \item{EntityName}{name of the irrigation entity.}
#'         \item{YearMonth}{calendar year and month YYYYMM.}
#'         \item{SWDiv}{surface-water diversions, in cubic meters per month.}
#'         \item{SeepFrac}{canal seepage as a fraction of diversions, a dimensionless quantity.}
#'         \item{CanalSeep}{canal seepage, in cubic meters per month.}
#'         \item{SWDel}{surface-water delivered to field headgates, in cubic meters per month.}
#'         \item{area.sw}{area irrigated by only surface water, in square meters.}
#'         \item{et.sw}{evapotranspiration on lands irrigated by only surface water,
#'           in cubic meters per month.}
#'         \item{precip.sw}{precipitation on lands irrigated by only surface water,
#'           in cubic meters per month.}
#'         \item{cir.sw}{crop irrigation requirement on lands irrigated by only surface water,
#'           in cubic meters per month.}
#'         \item{area.mix}{area irrigated by both surface and groundwater, in square meters.}
#'         \item{et.mix}{evapotranspiration on lands irrigated by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{precip.mix}{precipitation on lands irrigated by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{cir.mix}{crop irrigation requirement on lands irrigated by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{area.gw}{area irrigated by only groundwater, in square meters.}
#'         \item{et.gw}{evapotranspiration on lands irrigated by only groundwater,
#'           in cubic meters per month.}
#'         \item{precip.gw}{precipitation on lands irrigated by only groundwater,
#'           in cubic meters per month.}
#'         \item{cir.gw}{crop irrigation requirement on lands irrigated by only groundwater,
#'           in cubic meters per month.}
#'         \item{Eff}{irrigation efficiency, a dimensionless quantity.}
#'         \item{GWDiv}{recorded groundwater diversions, in cubic meters per month.}
#'         \item{WWDiv}{inflow to municipal wastewater treatment plants, in cubic meters per month.}
#'         \item{hg.sw}{surface-water delivered to field headgates on lands irrigated by only surface water,
#'           in cubic meters per month.}
#'         \item{hg.mix}{surface-water delivered to field headgates on lands irrigation by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{rech.sw}{incidental groundwater recharge beneath lands irrigated by only surface water,
#'           in cubic meters per month.}
#'         \item{gw.dem.mix}{groundwater demand on lands irrigated by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{gw.div.est}{calculated groundwater diversions, in cubic meters per month.}
#'         \item{rech.mix}{incidental groundwater recharge beneath lands irrigated by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{gw.only}{groundwater demand on lands irrigated by only groundwater in entities with
#'           lands also irrigated by both surface and groundwater, in cubic meters per month.}
#'         \item{rech.muni}{incidental groundwater recharge beneath entities with
#'           lands irrigated by only groundwater and lands irrigated by both surface and groundwater,
#'           in cubic meters per month.}
#'         \item{gw.dem.gw}{groundwater demand on lands irrigated by only groundwater in
#'           entities without surface-water irrigation, in cubic meters per month.}
#'         \item{rech.gw}{incidental groundwater recharge beneath lands irrigated by only groundwater,
#'           in cubic meters per month.}
#'         \item{area.model}{area of the irrigation entity that is located in the model domain,
#'           in square meters.}
#'       }
#'      Volumetric flow rates are calculated for their respective area in
#'      the irrigation entity---not just that part overlying the model area.
#'      Flow rate values are given this way in order to facilitate with quality assurance of
#'      the water-budget calculation.
#'      To calculate a simulated volumetric-flow rate: divide the flow rate by the affected area,
#'      and then multiply this value by the area of the irrigation entity that is located in
#'      the model domain (\code{area.model}).
#'
#'   (5) Well pumping data are also stored in \code{pumping.rech} (see \code{pod.rech} component),
#'       an object of data.frame class with the following components:
#'       \describe{
#'         \item{WMISNumber}{unique number assigned to a water right point of diversion.}
#'         \item{YearMonth}{calendar year and month YYYYMM.}
#'         \item{Pumping}{volumetric rate of pumping, in cubic meters per month.}
#'       }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#'   J. Sukow and M. McVay, Idaho Department of Water Resources
#'
#' @seealso \code{\link{UpdateWaterBudget}}
#'
#' @keywords manip
#'
#' @import sp
#' @import raster
#'
#' @export
#'
#' @examples
#' \dontrun{# see Appendix A. Package Introduction}
#'

RunWaterBalance <- function(r.grid, tr.stress.periods,
                            ss.stress.periods=NULL,
                            canal.seep=wrv::canal.seep,
                            comb.sw.irr=wrv::comb.sw.irr,
                            div.gw=wrv::div.gw,
                            div.sw=wrv::div.sw,
                            div.ww=wrv::div.ww,
                            efficiency=wrv::efficiency,
                            entity.components=wrv::entity.components,
                            et=wrv::et,
                            irr.entities=wrv::irr.entities,
                            land.surface=wrv::land.surface,
                            pod.gw=wrv::pod.gw,
                            priority.cuts=wrv::priority.cuts,
                            r.canals=wrv::r.canals,
                            rs.entities=wrv::rs.entities,
                            rs.rech.non.irr=wrv::rs.rech.non.irr,
                            verbose=FALSE) {

  yr.mo <- format(utils::head(tr.stress.periods, -1), "%Y%m")
  yr.mo.irr <- yr.mo[months(tr.stress.periods, abbreviate=TRUE) %in%
                     c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")]
  yr.mo.non.irr <- yr.mo[!yr.mo %in% yr.mo.irr]

  is.ss <- if (inherits(ss.stress.periods, "Date")) TRUE else FALSE

  # 3. diversions

  ## 3a. surface-water diversion

  FUN <- function(i) {
    d <- div.sw[div.sw$YearMonth == i, , drop=FALSE]
    d <- dplyr::summarise_(dplyr::group_by_(d, "EntityName"),
                           SWDiv="sum(SWDiv, na.rm=TRUE)")
    return(d)
  }
  sw.div.by.entity <- lapply(yr.mo, FUN)
  names(sw.div.by.entity) <- yr.mo

  ## 3b. gw diversions; water measurement information system (wmis)

  wmis.no <- sort(unique(pod.gw$WMISNumber))
  wmis.no.by.entity <- as.data.frame(list(WMISNumber=wmis.no))
  idxs <- match(wmis.no, pod.gw$WMISNumber)
  wmis.no.by.entity$EntityName <- pod.gw$EntityName[idxs]

  div.gw$id <- paste0(div.gw$WMISNumber, div.gw$YearMonth)
  div.gw.agg <- dplyr::summarise_(dplyr::group_by_(div.gw, "id"),
                                  GWDiv="sum(GWDiv, na.rm=TRUE)")

  d <- as.data.frame(list(WMISNumber=rep(wmis.no, each=length(yr.mo)),
                          YearMonth=rep(yr.mo, times=length(wmis.no)), GWDiv=0))
  idxs <- match(paste0(d$WMISNumber, d$YearMonth), div.gw.agg$id)
  d$GWDiv[!is.na(idxs)] <- div.gw.agg$GWDiv[stats::na.omit(idxs)]
  gw.div.by.wmis.no <- d

  FUN <- function(i) {
    is.neg <- gw.div.by.wmis.no$GWDiv < 0 & gw.div.by.wmis.no$YearMonth == i
    d <- gw.div.by.wmis.no[is.neg, ]
    d <- dplyr::summarise_(dplyr::group_by_(d, "WMISNumber"),
                           GWDiv="sum(GWDiv, na.rm=TRUE)")
    d <- dplyr::left_join(d, wmis.no.by.entity, by="WMISNumber")
    d <- dplyr::summarise_(dplyr::group_by_(d, "EntityName"),
                           GWDiv="sum(GWDiv, na.rm=TRUE)")
    return(d)
  }
  gw.div.by.entity <- lapply(yr.mo, FUN)
  names(gw.div.by.entity) <- yr.mo

  ## 3c. wastewater treament plant discharge

  FUN <- function(i) {
    d <- div.ww[div.ww$YearMonth == i, ]
    d <- dplyr::summarise_(dplyr::group_by_(d, "EntityName"),
                           WWDiv="sum(WWDiv, na.rm=TRUE)")
    return(d)
  }
  ww.div.by.entity <- lapply(yr.mo, FUN)
  names(ww.div.by.entity) <- yr.mo

  ## 3d. calculate estimates for recharge and groundwater

  FUN <- function(i) {
    irr <- irr.entities@data

    d <- data.frame(EntityName=levels(irr$EntityName))
    d <- suppressWarnings(dplyr::left_join(d, sw.div.by.entity[[i]], by="EntityName"))
    d$SWDiv[is.na(d$SWDiv)] <- 0

    d <- suppressWarnings(dplyr::left_join(d, canal.seep, by="EntityName"))
    d$CanalSeep <- d$SWDiv * d$SeepFrac
    d$CanalSeep[is.na(d$CanalSeep)] <- 0
    d$SWDel <- d$SWDiv - d$CanalSeep

    is.sw  <- d$EntityName %in% irr$EntityName[irr$Source == "SW Only"]
    is.mix <- d$EntityName %in% irr$EntityName[irr$Source == "Mixed"]
    is.gw  <- d$EntityName %in% irr$EntityName[irr$Source == "GW Only"]

    cols <- c("EntityName", "area", "et.vol", "precip.vol", "cir.vol")

    comp <- entity.components[[i]]@data
    comp <- comp[comp$Source == "SW Only", cols]
    names(comp) <- c("EntityName", "area.sw", "et.sw", "precip.sw", "cir.sw")
    d <- suppressWarnings(dplyr::left_join(d, comp, by="EntityName"))
    d$cir.sw[is.na(d$cir.sw)] <- 0

    comp <- entity.components[[i]]@data
    comp <- comp[comp$Source == "Mixed", cols]
    names(comp) <- c("EntityName", "area.mix", "et.mix", "precip.mix", "cir.mix")
    d <- suppressWarnings(dplyr::left_join(d, comp, by="EntityName"))
    d$cir.mix[is.na(d$cir.mix)] <- 0

    comp <- entity.components[[i]]@data
    comp <- comp[comp$Source == "GW Only", cols]
    names(comp) <- c("EntityName", "area.gw", "et.gw", "precip.gw", "cir.gw")
    d <- suppressWarnings(dplyr::left_join(d, comp, by="EntityName"))
    d$cir.gw[is.na(d$cir.gw)] <- 0

    efficiency <- efficiency[, c("EntityName", "Eff")]
    d <- suppressWarnings(dplyr::left_join(d, efficiency, by="EntityName"))
    d <- suppressWarnings(dplyr::left_join(d, gw.div.by.entity[[i]], by="EntityName"))
    d$GWDiv[is.na(d$GWDiv)] <- 0
    d <- suppressWarnings(dplyr::left_join(d, ww.div.by.entity[[i]], by="EntityName"))
    d$WWDiv[is.na(d$WWDiv)] <- 0
    cols <- c("hg.sw", "hg.mix", "rech.sw", "gw.dem.mix", "gw.div.est",
              "rech.mix", "gw.only", "rech.muni", "gw.dem.gw", "rech.gw")
    d[, cols] <- 0

    # sw only and mixed sources
    is.src <- is.sw & is.mix & !is.gw
    d$hg.sw[is.src] <- d$cir.sw[is.src] / d$Eff[is.src]
    d$hg.sw[is.src & d$cir.sw <= 0] <- 0
    d$hg.mix[is.src] <- d$SWDel[is.src] - d$hg.sw[is.src]
    is.neg <- is.src & d$hg.mix < 0
    d$hg.sw[is.neg] <- d$SWDel[is.neg]
    d$hg.mix[is.neg] <- 0
    d$rech.sw[is.src] <- d$hg.sw[is.src] - d$cir.sw[is.src]
    d$gw.dem.mix[is.src] <- d$cir.mix[is.src] / d$Eff[is.src] - d$hg.mix[is.src]
    d$gw.dem.mix[is.src & d$cir.mix <= 0] <- 0
    d$gw.div.est[is.src] <- -d$gw.dem.mix[is.src] - d$GWDiv[is.src]
    is.pos <- is.src & d$gw.div.est > 0
    d$gw.div.est[is.pos] <- 0
    d$rech.mix[is.src] <- d$hg.mix[is.src] - d$GWDiv[is.src] -
                          d$gw.div.est[is.src] - d$cir.mix[is.src]

    # mixed sources
    is.src <- !is.sw & is.mix & !is.gw
    d$hg.mix[is.src] <- d$SWDel[is.src]
    d$gw.dem.mix[is.src] <- d$cir.mix[is.src] / d$Eff[is.src] - d$hg.mix[is.src]
    d$gw.dem.mix[is.src & d$cir.mix <= 0] <- 0
    d$gw.div.est[is.src] <- d$WWDiv[is.src] - d$gw.dem.mix[is.src] - d$GWDiv[is.src]
    is.pos <- is.src & d$gw.div.est >= 0
    d$gw.div.est[is.pos] <- 0
    d$rech.mix[is.src] <- d$hg.mix[is.src] - d$GWDiv[is.src] + d$WWDiv[is.src] -
                          d$gw.div.est[is.src] - d$cir.mix[is.src]

    # sw only sources
    is.src <- is.sw & !is.mix & !is.gw
    d$hg.sw[is.src] <- d$SWDel[is.src]
    d$rech.sw[is.src] <- d$hg.sw[is.src] - d$cir.sw[is.src]

    # gw only and mixed sources (municipal systems)
    is.src <- !is.sw & is.mix & is.gw
    d$hg.mix[is.src] <- d$SWDel[is.src]
    d$gw.dem.mix[is.src] <- d$cir.mix[is.src] / d$Eff[is.src] - d$hg.mix[is.src]
    d$gw.dem.mix[is.src & d$cir.mix <= 0] <- 0
    d$gw.dem.mix[is.src & d$gw.dem.mix < 0] <- 0
    d$gw.only[is.src] <- d$cir.gw[is.src] / d$Eff[is.src]
    d$gw.only[is.src & d$cir.gw <= 0] <- 0
    d$gw.div.est[is.src] <- d$WWDiv[is.src] - d$gw.dem.mix[is.src] -
                            d$gw.only[is.src] - d$GWDiv[is.src]
    is.pos <- is.src & d$gw.div.est > 0
    d$gw.div.est[is.pos] <- 0
    d$rech.muni[is.src] <- d$hg.mix[is.src] - d$GWDiv[is.src] -
                           d$gw.div.est[is.src] + d$WWDiv[is.src] -
                           d$cir.mix[is.src] - d$cir.gw[is.src]

    # gw only sources
    is.src <- !is.sw & !is.mix & is.gw
    d$gw.dem.gw[is.src] <- d$cir.gw[is.src] / d$Eff[is.src]
    d$gw.dem.gw[is.src & d$cir.gw <= 0] <- 0
    d$gw.div.est[is.src] <- -d$gw.dem.gw[is.src] - d$GWDiv[is.src]
    d$gw.div.est[is.src & d$gw.div.est > 0] <- 0
    d$rech.gw[is.src] <- -d$GWDiv[is.src] - d$gw.div.est[is.src] - d$cir.gw[is.src]

    return(d)
  }
  div.by.entity <- lapply(yr.mo.irr, FUN)
  names(div.by.entity) <- yr.mo.irr

  cols <- names(div.by.entity[[1]])
  FUN <- function(i) {
    d <- data.frame(EntityName=levels(irr.entities@data$EntityName))
    d <- suppressWarnings(dplyr::left_join(d, sw.div.by.entity[[i]], by="EntityName"))
    d$SWDiv[is.na(d$SWDiv)] <- 0
    d <- suppressWarnings(dplyr::left_join(d, gw.div.by.entity[[i]], by="EntityName"))
    d$GWDiv[is.na(d$GWDiv)] <- 0
    d <- suppressWarnings(dplyr::left_join(d, ww.div.by.entity[[i]], by="EntityName"))
    d$WWDiv[is.na(d$WWDiv)] <- 0
    d$rech.gw <- d$SWDiv - d$GWDiv + d$WWDiv
    d[, cols[!cols %in% names(d)]] <- NA
    return(d[, cols])
  }
  l <- lapply(yr.mo.non.irr, FUN)
  names(l) <- yr.mo.non.irr
  div.by.entity <- c(div.by.entity, l)[yr.mo]

  ##

  if (verbose) {
    cell.area <- xres(land.surface) * yres(land.surface)

    cols <- names(div.by.entity[[1]])
    d <- dplyr::bind_rows(lapply(div.by.entity, function(i) i[, cols]))
    year.month <- rep(names(div.by.entity), times=vapply(div.by.entity, nrow, 0L))
    d <- cbind(YearMonth=year.month, d)
    rownames(d) <- NULL
    d <- d[order(d$EntityName), c(2, 1, 3:ncol(d))]
    cell.count <- rep(NA, nrow(d))
    for (i in yr.mo) {
      r <- mask(crop(rs.entities[[i]], r.grid), r.grid)
      rat <- levels(r)[[1]]
      rat$COUNT <- freq(r, useNA="no")[, "count"]
      is <- d$YearMonth == i
      cell.count[is] <- rat$COUNT[match(d$EntityName, rat$EntityName)][is]
    }
    d$area.model <- cell.area * cell.count
    inciden.rech <- as.data.frame(d)

    d <- data.frame(YearMonth=yr.mo, Area=NA, ET=NA, Rech=NA)
    row.names(d) <- yr.mo
    for (i in yr.mo) {
      r.rech <- crop(rs.rech.non.irr[[i]], r.grid)
      r.et <- crop(et[[i]], r.grid) * cell.area  # convert from length to vol.
      cells <- which(!is.na(r.grid[]) & !is.na(r.rech[]))
      d[i, "Area"] <- length(cells) * cell.area
      d[i, "ET"]   <- sum(r.et[cells])
      d[i, "Rech"] <- sum(r.rech[cells])
    }
    natural.rech <- as.data.frame(d)

  } else {
    inciden.rech <- NULL
    natural.rech <- NULL
  }

  ##

  FUN <- function(i) {
    d <- div.by.entity[[i]][, c("EntityName", "CanalSeep")]
    d <- d[d$CanalSeep != 0, ]
    r <- r.canals
    rat <- dplyr::left_join(levels(r)[[1]], d, by="EntityName")
    rat$CanalSeep <- rat$CanalSeep / rat$COUNT
    levels(r) <- rat[, c("ID", "CanalSeep")]
    r <- subs(r, levels(r)[[1]], subsWithNA=FALSE)
    return(r)
  }
  rs.rech.canals <- stack(lapply(yr.mo, FUN), quick=TRUE)
  names(rs.rech.canals) <- yr.mo

  FUN <- function(i) {
    r <- rs.entities[[i]]
    cols <- c("rech.mix", "rech.sw", "rech.muni", "rech.gw")
    d <- dplyr::left_join(levels(r)[[1]], div.by.entity[[i]][, c("EntityName", cols)],
                          by="EntityName")
    d$rech <- rowSums(d[, cols], na.rm=TRUE) / d$COUNT
    levels(r) <- d[, c("ID", "rech")]
    r <- subs(r, levels(r)[[1]], subsWithNA=FALSE)
    return(r)
  }
  rs.rech.irr <- stack(lapply(yr.mo, FUN), quick=TRUE)
  names(rs.rech.irr) <- yr.mo

  ##

  FUN <- function(i) {
    r <- sum(rs.rech.canals[[i]], rs.rech.irr[[i]], rs.rech.non.irr[[i]],
             na.rm=TRUE) * (1 / inlmisc::GetDaysInMonth(i))  # m^3/month to m^3/day
    return(r)
  }
  areal.rech <- crop(stack(lapply(yr.mo, FUN), quick=TRUE), r.grid)
  areal.rech <- mask(areal.rech, r.grid)
  names(areal.rech) <- yr.mo

  ## 3e. apportionment of estimated groundwater pumping to points of diversion

  sc.sources <- c("BUHLER DRAIN", "UNNAMED DRAIN", "UNNAMED STREAM",
                  "CAIN CREEK", "LOVING CREEK", "SILVER CREEK")
  is.sc.src <- comb.sw.irr$Source %in% sc.sources

  FUN <- function(i) {
    d <- comb.sw.irr
    d$sw.rate <- 0
    priority.cut <- priority.cuts[priority.cuts$YearMonth == i, "Pdate_BWR"]
    is.lt <- !is.sc.src & (!is.na(priority.cut) & d$Pdate < priority.cut)
    d$sw.rate[is.lt] <- d$MaxDivRate[is.lt]
    priority.cut <- priority.cuts[priority.cuts$YearMonth == i, "Pdate_SC"]
    is.lt <- is.sc.src & (!is.na(priority.cut) & d$Pdate < priority.cut)
    d$sw.rate[is.lt] <- d$MaxDivRate[is.lt]
    d <- dplyr::summarise_(dplyr::group_by_(d, "WaterRight"),
                           MaxDivRate="sum(MaxDivRate, na.rm=TRUE)",
                           sw.rate="sum(sw.rate, na.rm=TRUE)")
    d$sw.curt <- 1 - d$sw.rate / d$MaxDivRate
    d <- suppressWarnings(dplyr::left_join(pod.gw, d, by="WaterRight"))
    d$gw.rate <- d$IrrRate
    is.na.sw.curt <- is.na(d$sw.curt)
    d$gw.rate[!is.na.sw.curt] <- d$IrrRate[!is.na.sw.curt] *
                                 d$sw.curt[!is.na.sw.curt]

    is.est <- !d$WMISNumber %in% div.gw[div.gw$YearMonth == i, "WMISNumber"]
    d <- d[is.est, ]
    d.agg <- dplyr::summarise_(dplyr::group_by_(d, "EntityName"),
                               gw.rate="sum(gw.rate, na.rm=TRUE)")
    d$fraction <- d$gw.rate / d.agg$gw.rate[match(d$EntityName, d.agg$EntityName)]
    d$gw.div <- 0
    div <- div.by.entity[[i]][, c("EntityName", "gw.div.est")]
    idxs <- match(d$EntityName, div$EntityName)
    d$gw.div[!is.na(idxs)] <- d$fraction[!is.na(idxs)] *
                              div$gw.div.est[stats::na.omit(idxs)]
    return(d)
  }
  rech.by.pod <- lapply(yr.mo.irr, FUN)
  names(rech.by.pod) <- yr.mo.irr

  FUN <- function(i) {
    rec <- gw.div.by.wmis.no[gw.div.by.wmis.no$YearMonth == i, c("WMISNumber", "GWDiv")]
    est <- rech.by.pod[[i]][, c("WMISNumber", "gw.div")]
    est <- dplyr::summarise_(dplyr::group_by_(est, "WMISNumber"),
                             gw.div="sum(gw.div, na.rm=TRUE)")
    d <- merge(rec, est, all=TRUE, by="WMISNumber")
    d[[i]] <- rowSums(d[, c("GWDiv", "gw.div")], na.rm=TRUE)
    d <- d[, c("WMISNumber", i)]
    return(d)
  }
  pod.rech <- Reduce(function(...) merge(..., all=TRUE, by="WMISNumber"),
                     lapply(names(rech.by.pod), FUN))
  pod.rech[, yr.mo.non.irr] <- 0
  pod.rech <- pod.rech[, c("WMISNumber", yr.mo)]
  row.names(pod.rech) <- pod.rech$WMISNumber

  is.non.irr <- div.gw$YearMonth %in% yr.mo.non.irr
  d <- div.gw[is.non.irr, c("WMISNumber", "YearMonth", "GWDiv")]
  d <- aggregate(d$GWDiv, by=list(paste(d[, 1], d[, 2])), sum)
  d <- data.frame(do.call(rbind, strsplit(d[, 1], split=" ")), d[, 2])
  rows <- match(d[, 1], rownames(pod.rech))
  cols <- match(d[, 2], colnames(pod.rech))
  pod.rech[cbind(rows, cols)] <- d[, 3]

  ##

  if (verbose) {
    FUN <- function(i) {
      pump <- as.numeric(pod.rech[pod.rech$WMISNumber == i, yr.mo])
      d <- data.frame(Pumping=pump, WMISNumber=i, YearMonth=yr.mo)
      return(d[, c(2, 3, 1)])
    }
    d <- do.call("rbind", lapply(pod.rech$WMISNumber, FUN))
    pumping.rech <- d
  } else {
    pumping.rech <- NULL
  }

  ##

  FUN <- function(i) {pod.rech[[i]] * (1 / inlmisc::GetDaysInMonth(i))}  # m3/mo to m3/d
  pod.rech[, -1] <- data.frame(lapply(names(pod.rech)[-1], FUN))

  ##

  if (is.ss) {
    ss.yr.mo <- format(utils::head(ss.stress.periods, -1), "%Y%m")
    r <- calc(subset(areal.rech, ss.yr.mo), fun=mean)
    names(r) <- "ss"
    areal.rech <- stack(areal.rech, r, quick=TRUE)
    pod.rech$ss <- rowMeans(pod.rech[, ss.yr.mo], na.rm=TRUE)
  }

  ##

  invisible(list(areal.rech=areal.rech,
                 pod.rech=pod.rech,
                 natural.rech=natural.rech,
                 inciden.rech=inciden.rech,
                 pumping.rech=pumping.rech))
}
