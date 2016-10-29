#' Update Water Budget
#'
#' This function determines the specified-flow boundary conditions for the groundwater-flow model.
#' These boundary conditions include:
#'   (1) natural and incidental groundwater recharge at the water table,
#'   (2) groundwater pumping at production wells, and
#'   (3) groundwater underflow in the major tributary valleys.
#' Specified-flow values are saved to disk by rewriting the
#' \href{http://water.usgs.gov/ogw/modflow/}{MODFLOW} Well Package file (\file{.wel}).
#' Note that this function is executed after each iteration of \href{http://www.pesthomepage.org/}{PEST}.
#  It may also be run in an interactive \R session to initialize model parameter files.
#'
#' @param dir.run character.
#'   Path name of the directory to read/write model files.
#' @param id character.
#'   Short identifier (file name) for model files.
#' @param qa.tables character.
#'   Indicates if quality assurance tables are written to disk;
#'   by default "none" of these tables are written.
#'   Values of "si" and "english" indicate that table values are written in
#'   metric and English units, respectively.
#' @param ss.interval Date or character.
#'   Vector of length 2 specifying the start and end dates for the period used to
#'   represent steady-state boundary conditions.
#'   That is, recharge values for stress periods coinciding with this time period are
#'   averaged and used as steady-state boundary conditions.
#'   The required date format is YYYY-MM-DD.
#'   This argument overrides the \code{ss.stress.periods} object in the \file{model.rda} file,
#'   see \sQuote{Details} section for additional information.
#' @param iwelcb integer.
#'   A flag and unit number.
#'   If equal to zero, the default, cell-by-cell flow terms resulting from
#'   conditions in the MODFLOW Well Package will not be written to disk.
#'   A value of 0 is appropriate for model calibration,
#'   where MODFLOW run times are kept as short as possible.
#'   If greater than zero, the cell-by-cell flow terms are written to disk.
#'   See the MODFLOW Name File (\file{*.nam}) for the unit number associated with
#'   the budget file (\file{*.bud}).
#'   The default value is 50 (value specified in the \code{\link{WriteModflowInput}} function)
#'   if \R is being used interactively and 0 otherwise.
#' @param canal.seep data.frame.
#'   See \code{\link{canal.seep}} dataset for details.
#' @param efficiency data.frame.
#'   See \code{\link{efficiency}} dataset for details.
#' @param gage.disch data.frame.
#'   See \code{\link{gage.disch}} dataset for details.
#' @param pod.wells SpatialPointsDataFrame.
#'   See \code{\link{pod.wells}} dataset for details.
#' @param tributaries SpatialPolygonsDataFrame.
#'   See \code{\link{tributaries}} dataset for details.
#' @param ...
#'   Arguments to be passed to \code{\link{RunWaterBalance}}, such as evapotranspiration \code{et}.
#'
#' @details Files read during execution, and located within the \code{dir.run} directory,
#'   inlcude the MODFLOW hydraulic conductivity reference files \file{hk1.ref},
#'   \file{hk2.ref}, and \file{hk3.ref} corresponding to model layers 1, 2, and 3, respectively.
#'   Hydraulic conductivity values are read from a two-dimensional array in
#'   matrix format with \sQuote{white-space} delimited fields.
#'   And a binary data file \file{model.rda} containing the following serialized \R objects:
#'   \code{rs}, \code{misc}, \code{trib}, \code{tr.stress.periods}, and \code{ss.stress.periods}.
#'
#'   \code{rs} is an object of RasterStack class with raster layers \dQuote{lay1.top},
#'   \dQuote{lay1.bot}, \dQuote{lay2.bot}, and \dQuote{lay3.bot}.
#'   These raster layers describe the geometry of the model grid; that is,
#'   the upper and lower elevation of model layer 1, and the bottom elevations of model layers 2 and 3.
#'   Missing cell values (equal to NA) indicate inactive model cells lying outside of the model domain.
#'
#'   \code{misc} is a data.frame object with miscellaneous seepage,
#'   such as from the \sQuote{Bellevue Waste Water Treatment Plant ponds} and the \sQuote{Bypass Canal}.
#'   This object is comprised of the following components:
#'   \bold{lay}, \bold{row}, \bold{col} are integer values specifying a
#'   model cell's layer, row, and column index, respectively; and
#'   \bold{ss}, \bold{199501}, \bold{199502}, \dots, \bold{201012} are numeric values of
#'   elevation during each stress period, respectively,
#'   in meters above the North American Vertical Datum of 1988.
#'
#'   \code{trib} is a data.frame object with default values for the
#'   long-term mean underflows in each of the tributary basins.
#'   The object is comprised of the following components:
#'   \bold{Name} is a unique identifier for the tributary basin;
#'   \bold{lay}, \bold{row}, \bold{col} are \code{integer} values of a
#'   model cell's layer, row, and column index, respectively; and
#'   \bold{ss}, \bold{199501}, \bold{199502}, \dots, \bold{201012} are numeric values of
#'   underflow during each stress period, respectively, in cubic meters per day.
#'
#'   \code{tr.stress.periods} is a vector of Date values giving the start and end dates for
#'   stress periods in the model simulation period (1995--2010).
#'
#'   \code{ss.stress.periods} is a vector of Date values giving the start and end dates for
#'   stress periods used to define steady-state conditions.
#'
#'   \code{reduction} is a numeric default value for the signal amplitude reduction algorithm,
#'   a dimensionless quantity.
#'
#'   \code{d.in.mv.ave} is a numeric default value for the number of days in the
#'   moving average subset.
#'
#' @return Returns an object of difftime class, the runtime for this function.
#'   Used for the side-effect of files written to disk.
#'
#'   A MODFLOW Well Package file \file{<id>.wel} is always written to disk; whereas,
#'   parameter estimation files \file{seep.csv}, \file{eff.csv}, and \file{trib.csv}, and
#'   a script file \file{UpdateBudget.bat}, are only written if they do not already exist.
#'   The script file may be used to automate the execution of this function from a
#'   file manager (such as, Windows Explorer).
#'
#'   The \file{seep.csv} file stores as tabular data the canal seepage fraction for
#'   each of the irrigation entities.
#'   Its character and numeric data fields are delimited by commas (a comma-separated-value [CSV] file).
#'   The first line is reserved for field names \dQuote{EntityName} and \dQuote{SeepFrac}.
#'
#'   The \file{eff.csv} file stores as tabular data the irrigation efficiency for
#'   each of the irrigation entities.
#'   Its character and numeric data fields are delimited by commas.
#'   The first line is reserved for field names \dQuote{EntityName} and \dQuote{Eff}.
#'
#'   The \file{trib.csv} file stores as tabular data the underflow boundary conditions for
#'   each tributary basin.
#'   Its character and numeric data fields are delimited by commas.
#'   The first line is reserved for field names \dQuote{Name} and \dQuote{Value}.
#'   Data records include a long-term mean flow multiplier for
#'   each of the tributary basins (name is the unique identifier for the tributary),
#'   a record for the amplitude reduction (\code{reduction}), and
#'   a record for the number of days in the moving average (\code{d.in.mv.ave}).
#'
#'   If the \code{qa.tables} argument is specified as either \dQuote{si} or \dQuote{english},
#'   quality assurance tables are written to disk as CSV files (\file{qa-*.csv}).
#'   Volumetric flow rate data within these tables is described in the
#'   \sQuote{Value} section of the \code{\link{RunWaterBalance}} function;
#'   see returned list components \code{natural.rech}, \code{inciden.rech}, and \code{pumping.rech}.
#'   The well configuration data are described in the \sQuote{Value} section of the
#'   \code{\link{GetWellConfig}} function.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{RunWaterBalance}}, \code{\link{GetSeasonalMult}}
#'
#' @keywords utilities
#'
#' @import sp
#' @import raster
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dir.run <- file.path(getwd(), "model/model1")
#'   UpdateWaterBudget(dir.run, "wrv_mfusg", qa.tables = "si",
#'                     ss.interval = c("1998-01-01", "2011-01-01"))
#' }
#'

UpdateWaterBudget <- function(dir.run, id,
                              qa.tables=c("none", "si", "english"),
                              ss.interval=NULL,
                              iwelcb=ifelse(interactive(), 50L, 0L),
                              canal.seep=wrv::canal.seep,
                              efficiency=wrv::efficiency,
                              gage.disch=wrv::gage.disch,
                              pod.wells=wrv::pod.wells,
                              tributaries=wrv::tributaries,
                              ...) {

  # Initialize runtime

  t0 <- Sys.time()

  # Configure export of quality assurance tables

  qa.tables <- match.arg(qa.tables)
  verbose <- qa.tables != "none"
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
    utils::write.csv(canal.seep, file=f, quote=FALSE, row.names=FALSE)
  canal.seep <- utils::read.csv(f)

  # Read irrigation efficiency from calibration file

  f <- file.path(dir.run, "eff.csv")
  if (!file.exists(f))
    utils::write.csv(efficiency, file=f, quote=FALSE, row.names=FALSE)
  efficiency <- utils::read.csv(f)

  # Read tributary underflow control parameters from calibration file

  f <- file.path(dir.run, "trib.csv")
  if (!file.exists(f)) {
    d <- data.frame(Name=tributaries$Name, Value=1)
    d <- rbind(d, data.frame(Name=c("reduction", "d.in.mv.ave"),
                             Value=c(reduction, d.in.mv.ave)))
    utils::write.csv(d, file=f, quote=FALSE, row.names=FALSE)
  }
  d <- utils::read.csv(f)
  reduction   <- d$Value[d$Name == "reduction"]
  d.in.mv.ave <- d$Value[d$Name == "d.in.mv.ave"]
  scale.factors <- utils::head(d$Value, -2)
  names(scale.factors) <- utils::head(d$Name, -2)

  # Process specified flows in tributary canyons

  mult <- GetSeasonalMult(gage.disch[, c("Date", "13139510")], reduction,
                          d.in.mv.ave, tr.stress.periods)
  mult <- data.frame(Date=utils::head(tr.stress.periods, -1),
                     multiplier=rep(mult$multiplier, each=3))  # seasonal to monthly

  ave.flows <- tributaries$Flow
  names(ave.flows) <- tributaries$Name
  scale.factors <- scale.factors[match(names(scale.factors), names(ave.flows))]
  ave.flows <- ave.flows * scale.factors

  d <- t(vapply(ave.flows, function(i) mult$multiplier * i, rep(0, nrow(mult))))
  colnames(d) <- format(mult$Date, format="%Y%m")

  ss.yr.mo <- format(utils::head(ss.stress.periods, -1), "%Y%m")
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

  l <- RunWaterBalance(rs[["lay1.bot"]], tr.stress.periods,
                       ss.stress.periods=ss.stress.periods,
                       canal.seep=canal.seep, efficiency=efficiency,
                       verbose=verbose, ...)
  cells <- which(!is.na(l[["areal.rech"]][[1]][]))
  rc <- rowColFromCell(l[["areal.rech"]], cells)
  rech <- cbind(lay = 1, row = rc[, 1], col = rc[, 2], l[["areal.rech"]][cells])
  wells <- pod.wells[match(l[["pod.rech"]]$WMISNumber, pod.wells@data$WMISNumber), ]
  wells@data <- dplyr::left_join(wells@data, l[["pod.rech"]], by="WMISNumber")
  well <- GetWellConfig(rs, wells, "WMISNumber", names(l[["pod.rech"]][-1]))
  well.config <- well[, 1:7]

  # Combine recharge components into a single data frame

  yr.mo <- c("ss", format(utils::head(tr.stress.periods, -1), "%Y%m"))

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
    utils::write.table(mm, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                       col.names=FALSE)
  }

  # Write batch file to run UpdateWaterBudget function

  f <- file.path(dir.run, "UpdateBudget.bat")
  if (!file.exists(f)) {
    cmd <- shQuote(paste0(file.path(R.home(component="bin"), "Rscript")))
    cmd <- paste(cmd, "--vanilla --default-packages=utils,stats,raster,wrv -e")
    cmd <- paste(cmd, shQuote(paste0("UpdateWaterBudget(", shQuote("."), ", ",
                                     shQuote(id), ")")))
    cat(cmd, file=f, sep="\n")
    Sys.chmod(f, mode="755")
  }

  # Write recharge quality assurance tables

  if (verbose) {
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
    utils::write.csv(d, file=f, quote=FALSE, row.names=FALSE)

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
    utils::write.csv(d, file=f, quote=FALSE, row.names=FALSE)

    d <- l[["pumping.rech"]]
    if (qa.tables == "english") {
      d$Pumping <- d$Pumping * m3.to.af
      names(d)[3] <- paste0(names(d)[3], "_af")
    } else {
      names(d)[3] <- paste0(names(d)[3], "_m3")
    }
    f <- file.path(dir.run, "qa-pumping.csv")
    utils::write.csv(d, file=f, quote=FALSE, row.names=FALSE)

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
    utils::write.csv(d, file=f, quote=FALSE, row.names=FALSE)
  }

  # Return runtime

  invisible(difftime(Sys.time(), t0))
}
