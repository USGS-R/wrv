UpdateWaterBudget <- function(dir.run, id,
                              qa.tables=c("none", "si", "english"),
                              ss.interval=NULL, iwelcb=0L) {

  # Initialize runtime

  t0 <- Sys.time()

  # Configure export of quality assurance tables

  qa.tables <- match.arg(qa.tables)
  qa.write <- qa.tables != "none"
  m2.to.acre <- 0.000247105
  m3.to.af   <- 0.000810713194
  m.to.ft    <- 3.28084

  # Load datasets created in the model vignette

  rs <- misc <- trib <- tr.stress.periods <- ss.stress.periods <- NULL
  f <- file.path(dir.run, "model.rda")
  load(f)

  # Determine number of stress periods from MODFLOW discretization package file

  f <- file.path(dir.run, paste0(id, ".dis"))
  nper <- scan(f, skip=2, nlines=1, comment.char="#", quiet=TRUE)[4]

  # Read canal seepage from calibration file

  f <- file.path(dir.run, "seep.csv")
  if (!file.exists(f))
    write.csv(wrv::canal.seep, file=f, quote=FALSE, row.names=FALSE)
  seep <- read.csv(f)

  # Read irrigation efficiency from calibration file

  f <- file.path(dir.run, "eff.csv")
  if (!file.exists(f))
    write.csv(wrv::efficiency, file=f, quote=FALSE, row.names=FALSE)
  eff <- read.csv(f)

  # Read tributary underflow from calibration file

  f <- file.path(dir.run, "trib.csv")
  if (!file.exists(f)) {
    d <- data.frame(Name=wrv::tributaries$Name, Value=1)
    d <- rbind(d, data.frame(Name=c("reduction", "d.in.mv.ave"),
                             Value=c(reduction, d.in.mv.ave)))
    write.csv(d, file=f, quote=FALSE, row.names=FALSE)
  }
  d <- read.csv(f)
  reduction   <- d$Value[d$Name == "reduction"]
  d.in.mv.ave <- d$Value[d$Name == "d.in.mv.ave"]
  scale.factors <- head(d$Value, -2)
  names(scale.factors) <- head(d$Name, -2)

  # Process specified flows in tributary canyons

  mult <- GetSeasonalMult(wrv::gage.disch[, c("Date", "13139510")], reduction,
                          d.in.mv.ave, tr.stress.periods)
  mult <- data.frame(Date=head(tr.stress.periods, -1),
                     multiplier=rep(mult$multiplier, each=3))

  ave.flows <- wrv::tributaries$Flow
  names(ave.flows) <- wrv::tributaries$Name
  scale.factors <- scale.factors[match(names(scale.factors), names(ave.flows))]
  ave.flows <- ave.flows * scale.factors

  d <- t(vapply(ave.flows, function(i) mult$multiplier * i, rep(0, nrow(mult))))
  colnames(d) <- format(mult$Date, format="%Y%m")

  ss.yr.mo <- format(head(ss.stress.periods, -1), "%Y%m")
  d <- cbind(d, ss=apply(d[, ss.yr.mo], 1, mean))
  trib[, colnames(d)] <- d[match(trib$Name, row.names(d)), ]
  trib[, colnames(d)] <- trib[, colnames(d)] / trib$count

  # Read hydraulic conductivity from reference files

  ncells <- length(rs[[1]])
  for (i in 1:3) {
    f <- file.path(dir.run, paste0("hk", i, ".ref"))
    r <- setValues(raster(rs), scan(f, n=ncells, quiet=TRUE))
    r[is.na(rs[[paste0("lay", i, ".bot")]])] <- NA
    names(r) <- paste0("lay", i, ".hk")
    rs <- stack(rs, r)
  }

  # Process areal recharge and pumping demand

  l <- RunWaterBalance(tr.stress.periods, rs[["lay1.bot"]], eff, seep,
                       ss.stress.periods=ss.stress.periods,
                       verbose=qa.write)
  cells <- which(!is.na(l[["areal.rech"]][[1]][]))
  rc <- rowColFromCell(l[["areal.rech"]], cells)
  rech <- cbind(lay = 1, row = rc[, 1], col = rc[, 2], l[["areal.rech"]][cells])
  wells <- wrv::pod.wells[match(l[["pod.rech"]]$WMISNumber,
                                wrv::pod.wells@data$WMISNumber), ]
  wells@data <- dplyr::left_join(wells@data, l[["pod.rech"]], by="WMISNumber")
  well <- GetWellConfig(rs, wells, "WMISNumber", names(l[["pod.rech"]][-1]))
  well.config <- well[, 1:7]

  # Combine recharge components into a single data frame

  yr.mo <- c("ss", format(head(tr.stress.periods, -1), "%Y%m"))

  cols <- c("lay", "row", "col", yr.mo)
  rech <- as.matrix(cbind(rech[, cols], id=1))
  well <- as.matrix(cbind(well[, cols], id=2))
  trib <- as.matrix(cbind(trib[, cols], id=3))
  misc <- as.matrix(cbind(misc[, cols], id=4))
  m <- rbind(rech, well, trib, misc)

  # Revise steady-state conditions

  if (!is.null(ss.interval) && length(ss.interval) == 2L) {
    ss.interval <- as.Date(ss.interval, tz = "MST")
    yr.mo.ss <- format(seq(ss.interval[1], ss.interval[2], "1 month"), "%Y%m")
    yr.mo.ss <- yr.mo.ss[yr.mo.ss %in% colnames(m)]
    m[, "ss"] <- rowMeans(m[, yr.mo.ss], na.rm=TRUE)
  }

  # Write MODFLOW well package file

  f <- file.path(dir.run, paste0(id, ".wel"))
  if (file.exists(f)) file.remove(f)

  ds.0 <- paste0("# Wood River Valley flow model (", Sys.time(), " ",
                 Sys.timezone(), ")\n# MODFLOW Well Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  ds.2 <- paste(nrow(m), iwelcb, "AUXILIARY id NOPRINT",
                " # MXACTW,IWELCB,[Option]")
  cat(ds.2, file=f, sep="\n", append=TRUE)  # well pumping not saved to budget

  for (i in yr.mo[seq_len(nper)]) {
    mm <- m[, c("lay", "row", "col", i, "id")]
    mm <- mm[mm[, i] != 0, , drop=FALSE]
    if (nrow(mm) == 0) next
    ds.5 <- paste(nrow(mm), 0, " # ITMP,NP    STRESS PERIOD", i)
    cat(ds.5, file=f, sep="\n", append=TRUE)
    write.table(mm, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                col.names=FALSE)
  }

  # Write batch file to run UpdateWaterBudget function

  f <- file.path(dir.run, "UpdateBudget.bat")
  if (!file.exists(f)) {
    cmd <- shQuote(paste0(file.path(R.home(component="bin"), "Rscript")))
    cmd <- paste(cmd, "--vanilla --default-packages=utils,stats,wrv -e")
    cmd <- paste(cmd, shQuote(paste0("UpdateWaterBudget(", shQuote("."), ", ",
                                     shQuote(id), ")")))
    cat(cmd, file=f, sep="\n")
    Sys.chmod(f, mode="755")
  }

  # Write recharge quality assurance tables

  if (qa.write) {
    d <- l[["inciden.rech"]]
    misc.cols <- c("EntityName", "YearMonth", "SeepFrac", "Eff")
    area.cols <- c("area.sw", "area.mix", "area.gw", "area.model")
    m2 <- match(area.cols, names(d))
    m3 <- which(!(names(d) %in% c(misc.cols, area.cols)))
    if (qa.tables == "english") {
      d[, m2] <- d[, m2] * m2.to.acre
      d[, m3] <- d[, m3] * m3.to.af
      names(d)[m2] <- paste0(names(d)[m2], "_acre")
      names(d)[m3] <- paste0(names(d)[m3], "_af")
    } else {
      names(d)[m2] <- paste0(names(d)[m2], "_m2")
      names(d)[m3] <- paste0(names(d)[m3], "_m3")
    }
    f <- file.path(dir.run, "qa-incidental.csv")
    write.csv(d, file=f, quote=FALSE, row.names=FALSE)

    d <- l[["natural.rech"]]
    cols <- match(c("Area", "ET", "Rech"), names(d))
    if (qa.tables == "english") {
      d$Area <- d$Area * m2.to.acre
      d$ET   <- d$ET   * m3.to.af
      d$Rech <- d$Rech * m3.to.af
      names(d)[cols] <- c("Area_acre", "ET_af", "Rech_af")
    } else {
      names(d)[cols] <- c("Area_m2", "ET_m3", "Rech_m3")
    }
    f <- file.path(dir.run, "qa-natural.csv")
    write.csv(d, file=f, quote=FALSE, row.names=FALSE)

    d <- l[["pumping.rech"]]
    if (qa.tables == "english") {
      d$Pumping <- d$Pumping * m3.to.af
      names(d)[3] <- paste0(names(d)[3], "_af")
    } else {
      names(d)[3] <- paste0(names(d)[3], "_m3")
    }
    f <- file.path(dir.run, "qa-pumping.csv")
    write.csv(d, file=f, quote=FALSE, row.names=FALSE)

    d <- well.config
    if (qa.tables == "english") {
      d$hk <- d$hk * m.to.ft
      names(d)[5] <- paste0(names(d)[5], "_ft.per.d")
      d$thk <- d$thk * m.to.ft
      names(d)[6] <- paste0(names(d)[6], "_ft")
    } else {
      names(d)[5] <- paste0(names(d)[5], "_m.per.d")
      names(d)[6] <- paste0(names(d)[6], "_m")
    }
    f <- file.path(dir.run, "qa-well-config.csv")
    write.csv(d, file=f, quote=FALSE, row.names=FALSE)
  }

  # Return runtime

  invisible(difftime(Sys.time(), t0))
}
