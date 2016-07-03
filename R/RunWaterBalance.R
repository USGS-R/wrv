RunWaterBalance <- function(tr.stress.periods, r.grid, eff, seep,
                            ss.stress.periods=NULL, verbose=FALSE) {

  yr.mo <- format(head(tr.stress.periods, -1), "%Y%m")
  yr.mo.irr <- yr.mo[months(tr.stress.periods, abbreviate=TRUE) %in%
                     c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")]
  yr.mo.non.irr <- yr.mo[!yr.mo %in% yr.mo.irr]

  is.ss <- if (inherits(ss.stress.periods, "Date")) TRUE else FALSE

  # 3. diversions

  ## 3a. surface-water diversion

  FUN <- function(i) {
    d <- wrv::div.sw[wrv::div.sw$YearMonth == i, , drop=FALSE]
    d <- summarise_(group_by_(d, "EntityName"), SWDiv="sum(SWDiv, na.rm=TRUE)")
    return(d)
  }
  sw.div.by.entity <- lapply(yr.mo, FUN)
  names(sw.div.by.entity) <- yr.mo

  ## 3b. gw diversions; water measurement information system (wmis)

  wmis.no <- sort(unique(wrv::pod.gw$WMISNumber))
  wmis.no.by.entity <- as.data.frame(list(WMISNumber=wmis.no))
  idxs <- match(wmis.no, wrv::pod.gw$WMISNumber)
  wmis.no.by.entity$EntityName <- wrv::pod.gw$EntityName[idxs]

  div.gw$id <- paste0(div.gw$WMISNumber, div.gw$YearMonth)
  div.gw.agg <- summarise_(group_by_(div.gw, "id"),
                           GWDiv="sum(GWDiv, na.rm=TRUE)")

  d <- as.data.frame(list(WMISNumber=rep(wmis.no, each=length(yr.mo)),
                          YearMonth=rep(yr.mo, times=length(wmis.no)), GWDiv=0))
  idxs <- match(paste0(d$WMISNumber, d$YearMonth), div.gw.agg$id)
  d$GWDiv[!is.na(idxs)] <- div.gw.agg$GWDiv[na.omit(idxs)]
  gw.div.by.wmis.no <- d

  FUN <- function(i) {
    is.neg <- gw.div.by.wmis.no$GWDiv < 0 & gw.div.by.wmis.no$YearMonth == i
    d <- gw.div.by.wmis.no[is.neg, ]
    d <- summarise_(group_by_(d, "WMISNumber"), GWDiv="sum(GWDiv, na.rm=TRUE)")
    d <- left_join(d, wmis.no.by.entity, by="WMISNumber")
    d <- summarise_(group_by_(d, "EntityName"), GWDiv="sum(GWDiv, na.rm=TRUE)")
    return(d)
  }
  gw.div.by.entity <- lapply(yr.mo, FUN)
  names(gw.div.by.entity) <- yr.mo

  ## 3c. wastewater treament plant discharge

  FUN <- function(i) {
    d <- wrv::div.ww[wrv::div.ww$YearMonth == i, ]
    d <- summarise_(group_by_(d, "EntityName"), WWDiv="sum(WWDiv, na.rm=TRUE)")
    return(d)
  }
  ww.div.by.entity <- lapply(yr.mo, FUN)
  names(ww.div.by.entity) <- yr.mo

  ## 3d. calculate estimates for recharge and groundwater

  FUN <- function(i) {
    irr <- wrv::irr.entities@data

    d <- data.frame(EntityName=levels(irr$EntityName))
    d <- suppressWarnings(left_join(d, sw.div.by.entity[[i]], by="EntityName"))
    d$SWDiv[is.na(d$SWDiv)] <- 0

    d <- suppressWarnings(left_join(d, seep, by="EntityName"))
    d$CanalSeep <- d$SWDiv * d$SeepFrac
    d$CanalSeep[is.na(d$CanalSeep)] <- 0
    d$SWDel <- d$SWDiv - d$CanalSeep

    is.sw  <- d$EntityName %in% irr$EntityName[irr$Source == "SW Only"]
    is.mix <- d$EntityName %in% irr$EntityName[irr$Source == "Mixed"]
    is.gw  <- d$EntityName %in% irr$EntityName[irr$Source == "GW Only"]

    cols <- c("EntityName", "area", "et.vol", "precip.vol", "cir.vol")

    comp <- wrv::entity.components[[i]]@data
    comp <- comp[comp$Source == "SW Only", cols]
    names(comp) <- c("EntityName", "area.sw", "et.sw", "precip.sw", "cir.sw")
    d <- suppressWarnings(left_join(d, comp, by="EntityName"))
    d$cir.sw[is.na(d$cir.sw)] <- 0

    comp <- wrv::entity.components[[i]]@data
    comp <- comp[comp$Source == "Mixed", cols]
    names(comp) <- c("EntityName", "area.mix", "et.mix", "precip.mix",
                     "cir.mix")
    d <- suppressWarnings(left_join(d, comp, by="EntityName"))
    d$cir.mix[is.na(d$cir.mix)] <- 0

    comp <- wrv::entity.components[[i]]@data
    comp <- comp[comp$Source == "GW Only", cols]
    names(comp) <- c("EntityName", "area.gw", "et.gw", "precip.gw", "cir.gw")
    d <- suppressWarnings(left_join(d, comp, by="EntityName"))
    d$cir.gw[is.na(d$cir.gw)] <- 0

    eff <- eff[, c("EntityName", "Eff")]
    d <- suppressWarnings(left_join(d, eff, by="EntityName"))
    d <- suppressWarnings(left_join(d, gw.div.by.entity[[i]], by="EntityName"))
    d$GWDiv[is.na(d$GWDiv)] <- 0
    d <- suppressWarnings(left_join(d, ww.div.by.entity[[i]], by="EntityName"))
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
    d$gw.div.est[is.src] <- d$WWDiv[is.src] - d$gw.dem.mix[is.src] -
                            d$GWDiv[is.src]
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
    d$rech.gw[is.src] <- -d$GWDiv[is.src] - d$gw.div.est[is.src] -
                         d$cir.gw[is.src]

    return(d)
  }
  div.by.entity <- lapply(yr.mo.irr, FUN)
  names(div.by.entity) <- yr.mo.irr

  cols <- names(div.by.entity[[1]])
  FUN <- function(i) {
    d <- data.frame(EntityName=levels(wrv::irr.entities@data$EntityName))

    d <- suppressWarnings(left_join(d, sw.div.by.entity[[i]], by="EntityName"))
    d$SWDiv[is.na(d$SWDiv)] <- 0

    d <- suppressWarnings(left_join(d, gw.div.by.entity[[i]], by="EntityName"))
    d$GWDiv[is.na(d$GWDiv)] <- 0

    d <- suppressWarnings(left_join(d, ww.div.by.entity[[i]], by="EntityName"))
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
    cell.area <- xres(wrv::land.surface) * yres(wrv::land.surface)

    cols <- names(div.by.entity[[1]])
    d <- bind_rows(lapply(div.by.entity, function(i) i[, cols]))
    year.month <- rep(names(div.by.entity),
                      times=vapply(div.by.entity, nrow, 0L))
    d <- cbind(YearMonth=year.month, d)
    rownames(d) <- NULL
    d <- d[order(d$EntityName), c(2, 1, 3:ncol(d))]
    cell.count <- rep(NA, nrow(d))
    for (i in yr.mo) {
      r <- mask(crop(wrv::rs.entities[[i]], r.grid), r.grid)
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
      r.rech <- crop(wrv::rs.rech.non.irr[[i]], r.grid)
      r.et <- crop(wrv::et[[i]], r.grid) * cell.area  # convert from length to vol.
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
    r <- wrv::r.canals
    rat <- left_join(levels(r)[[1]], d, by="EntityName")
    rat$CanalSeep <- rat$CanalSeep / rat$COUNT
    levels(r) <- rat[, c("ID", "CanalSeep")]
    r <- subs(r, levels(r)[[1]], subsWithNA=FALSE)
    return(r)
  }
  rs.rech.canals <- stack(lapply(yr.mo, FUN), quick=TRUE)
  names(rs.rech.canals) <- yr.mo

  FUN <- function(i) {
    r <- wrv::rs.entities[[i]]
    cols <- c("rech.mix", "rech.sw", "rech.muni", "rech.gw")
    d <- left_join(levels(r)[[1]], div.by.entity[[i]][, c("EntityName", cols)],
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
    r <- sum(rs.rech.canals[[i]], rs.rech.irr[[i]], wrv::rs.rech.non.irr[[i]],
             na.rm=TRUE) * (1 / GetDaysInMonth(i))  # m^3/month to m^3/day
    return(r)
  }
  areal.rech <- crop(stack(lapply(yr.mo, FUN), quick=TRUE), r.grid)
  areal.rech <- mask(areal.rech, r.grid)
  names(areal.rech) <- yr.mo

  ## 3e. apportionment of estimated groundwater pumping to points of diversion

  sc.sources <- c("BUHLER DRAIN", "UNNAMED DRAIN", "UNNAMED STREAM",
                  "CAIN CREEK", "LOVING CREEK", "SILVER CREEK")
  is.sc.src <- wrv::comb.sw.irr$Source %in% sc.sources

  FUN <- function(i) {
    d <- wrv::comb.sw.irr
    d$sw.rate <- 0
    priority.cut <- wrv::priority.cuts[wrv::priority.cuts$YearMonth == i, 
                                       "Pdate_BWR"]
    is.lt <- !is.sc.src & (!is.na(priority.cut) & d$Pdate < priority.cut)
    d$sw.rate[is.lt] <- d$MaxDivRate[is.lt]
    priority.cut <- wrv::priority.cuts[wrv::priority.cuts$YearMonth == i, 
                                       "Pdate_SC"]
    is.lt <- is.sc.src & (!is.na(priority.cut) & d$Pdate < priority.cut)
    d$sw.rate[is.lt] <- d$MaxDivRate[is.lt]
    d <- summarise_(group_by_(d, "WaterRight"),
                    MaxDivRate="sum(MaxDivRate, na.rm=TRUE)",
                    sw.rate="sum(sw.rate, na.rm=TRUE)")
    d$sw.curt <- 1 - d$sw.rate / d$MaxDivRate
    d <- suppressWarnings(left_join(wrv::pod.gw, d, by="WaterRight"))
    d$gw.rate <- d$IrrRate
    is.na.sw.curt <- is.na(d$sw.curt)
    d$gw.rate[!is.na.sw.curt] <- d$IrrRate[!is.na.sw.curt] *
                                 d$sw.curt[!is.na.sw.curt]

    is.est <- !d$WMISNumber %in% div.gw[div.gw$YearMonth == i, "WMISNumber"]
    d <- d[is.est, ]
    d.agg <- summarise_(group_by_(d, "EntityName"),
                        gw.rate="sum(gw.rate, na.rm=TRUE)")
    d$fraction <- d$gw.rate /
                  d.agg$gw.rate[match(d$EntityName, d.agg$EntityName)]
    d$gw.div <- 0
    div <- div.by.entity[[i]][, c("EntityName", "gw.div.est")]
    idxs <- match(d$EntityName, div$EntityName)
    d$gw.div[!is.na(idxs)] <- d$fraction[!is.na(idxs)] *
                              div$gw.div.est[na.omit(idxs)]
    return(d)
  }
  rech.by.pod <- lapply(yr.mo.irr, FUN)
  names(rech.by.pod) <- yr.mo.irr

  FUN <- function(i) {
    rec <- gw.div.by.wmis.no[gw.div.by.wmis.no$YearMonth == i,
                             c("WMISNumber", "GWDiv")]
    est <- rech.by.pod[[i]][, c("WMISNumber", "gw.div")]
    est <- summarise_(group_by_(est, "WMISNumber"),
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

  FUN <- function(i) {pod.rech[[i]] * (1 / GetDaysInMonth(i))}  # m3/mo to m3/d
  pod.rech[, -1] <- data.frame(lapply(names(pod.rech)[-1], FUN))

  ##

  if (is.ss) {
    ss.yr.mo <- format(head(ss.stress.periods, -1), "%Y%m")
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
