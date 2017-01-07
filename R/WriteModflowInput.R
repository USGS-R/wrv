#' Write MODFLOW Input Files
#'
#' This function generates and writes input files for a MODFLOW simulation of
#' groundwater flow in the Wood River Valley (WRV) aquifer system.
#'
#' @param rs.model RasterStack.
#'   Collection of RasterLayer objects with the same extent and resolution,
#'   see \sQuote{Details} for required raster layers.
#' @param rech data.frame.
#'   Areal recharge rate, in cubic meters per day.
#'   Variables describe the model cell location (\code{lay}, \code{row}, \code{col}) and
#'   volumetric rate during each stress period
#'   (\code{ss}, \code{199501}, \code{199502}, \dots, \code{201012}).
#' @param well data.frame.
#'   Well pumping at point locations in cubic meters per day.
#'   Variables describe the model cell location and volumetric rate during each stress period.
#' @param trib data.frame.
#'   Incoming flows from the major tributary canyons.
#'   Variables describe the model cell location and volumetric rate during each stress period.
#' @param misc data.frame.
#'   Direct recharge from miscellaneous seepage sites in cubic meters per day.
#'   Variables describe the model cell location and volumetric rate during each stress period.
#' @param river data.frame.
#'   River conditions.
#'   Variables describe the model cell location, river conductance
#'   (\code{cond}) in square meters per day, river bottom elevation (\code{bottom}) in
#'   meters above the North American Vertical Datum of 1988 (NAVD 88), and
#'   a numeric river reach identifier (\code{id}).
#' @param drain data.frame.
#'   Drain conditions for groundwater outlet boundaries.
#'   Variables describe the model cell location, drain threshold elevation
#'   (\code{elev}) in meters above the NAVD 88, drain conductance (\code{cond}) in
#'   square meters per day, and a numeric identifier (\code{id}) indicating the
#'   drains general location.
#' @param id character.
#'   Short identifier for the model run.
#' @param dir.run character.
#'   Path name of the directory to write model input files.
#' @param is.convertible logical.
#'   If true, indicates model layers are \sQuote{convertible}, with
#'   transmissivity computed using upstream water-table depth.
#'   Otherwise, model layers are \sQuote{confined} and transmissivity is constant over time.
#' @param ss.perlen integer or difftime.
#'   Length of the steady-state stress period in days.
#' @param tr.stress.periods Date.
#'   Vector of start times for each stress period in the transient simulation.
#'   If missing, only steady-state conditions are simulated.
#' @param ntime.steps integer.
#'   Number of uniform time steps in a stress period.
#' @param mv.flag numeric.
#'   Missing value flag for output reference data files.
#' @param auto.flow.reduce logical.
#'   If true, a simulated well will adjust pumping according to
#'   supply under bottom-hole conditions.
#'   Pumping rates that have been automatically reduced will be written to a
#'   model output file (\file{.afr}).
#' @param verbose logical.
#'   If true, additional information is written to the
#'   listing file (\file{.lst}) and budget file (\file{.bud})
#'
#' @details Groundwater flow in the WRV aquifer system is simulated using the
#'   \href{https://water.usgs.gov/ogw/mfusg/}{MODFLOW-USG} groundwater-flow model.
#'   This numerical model was chosen for its ability to solve
#'   complex unconfined groundwater flow simulations.
#'   The solver implemented in MODFLOW-USG incorporates the Newton-Raphson formulation for
#'   improving solution convergence and avoiding problems with the drying and
#'   rewetting of cells (Niswonger and others, 2011).
#'   A structured finite-difference grid is implemented in the model to
#'     (1) simplify discretization,
#'     (2) keep formats and structures for the MODFLOW-USG packages identical to those of
#'         \href{https://water.usgs.gov/ogw/modflow/MODFLOW-2005-Guide/index.html}{MODFLOW-2005}, and
#'     (3) allow any MODFLOW post-processor to be used to analyze the results of the MODFLOW-USG simulation
#'         (such as \href{https://water.usgs.gov/nrp/gwsoftware/modelviewer/ModelViewer.html}{Model Viewer}).
#'
#'   Model input files are written to \code{dir.run} and include the following MODFLOW Package files:
#'   Name (\file{.nam}), Basic (\file{.ba6}), Discretization (\file{.dis}),
#'   Layer-Property Flow (\file{.lpf}), Drain (\file{.drn}), River (\file{.riv}),
#'   Well (\file{.wel}), Sparse Matrix Solver (\file{.sms}), and Output Control (\file{.oc}).
#'   See the users guide (\cite{Description of Model Input and Output}) included with the MODFLOW-USG
#'   software for details on input file formats and structures.
#'
#'   Data within the \code{rech}, \code{well}, \code{trib}, and \code{misc} arguments are
#'   combined in the MODFLOW Well Package and identifiable with added \code{id} values of
#'   1, 2, 3, and 4, respectively.
#'
#'   The Layer-Property Flow file includes options for the calculation of vertical flow in
#'   partially dewatered cells.
#'   For the WRV model, where there is no indication that perched conditions exist,
#'   CONSTANTCV and NOVFC options are used to create the most stable solution
#'   (Panday and others, 2013, p. 15-16).
#'   Options for the Sparse Matrix Solver were set for unconfined simulations by
#'   implementing an upstream-weighting scheme with Newton-Raphson linearization,
#'   Delta-Bar-Delta under-relaxation, and the \eqn{\chi}MD solver of Ibaraki (2005).
#'
#'   The raster stack \code{rs.model} includes the following layers:
#'   \describe{
#'     \item{lay1.top}{elevation at the top of model layer 1 (land surface),
#'       in meters above the NAVD 88.}
#'     \item{lay1.bot}{elevation at the bottom of model layer 1, in meters above the NAVD 88.}
#'     \item{lay2.bot}{elevation at the bottom of model layer 2.}
#'     \item{lay3.bot}{elevation at the bottom of model layer 3.}
#'     \item{lay1.strt}{initial (starting) hydraulic head in model layer 1,
#'       in meters above the NAVD 88.}
#'     \item{lay2.strt}{initial hydraulic head in model layer 2.}
#'     \item{lay3.strt}{initial hydraulic head in model layer 3.}
#'     \item{lay1.zones}{hydrogeologic zones in model layer 1 where values
#'       equal to 1 is unconfined alluvium, equal to 2 is basalt,
#'       equal to 3 is clay, and equal to 4 is confined alluvium.}
#'     \item{lay2.zones}{hydrogeologic zones in model layer 2.}
#'     \item{lay3.zones}{hydrogeologic zones in model layer 3.}
#'     \item{lay1.hk}{horizontal hydraulic conductivity in model layer 1, in meters per day.}
#'     \item{lay2.hk}{horizontal hydraulic conductivity in model layer 2.}
#'     \item{lay3.hk}{horizontal hydraulic conductivity in model layer 3.}
#'   }
#'
#' @return Used for the side-effect of files written to disk.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references Ibaraki, M., 2005, \eqn{\chi}MD User's guide-An efficient sparse matrix solver library, version 1.30:
#'   Columbus, Ohio State University School of Earth Sciences.
#'
#'   Niswonger, R.G., Panday, Sorab, and Ibaraki, Motomu, 2011, MODFLOW-NWT, A Newton formulation for MODFLOW-2005:
#'   U.S. Geological Survey Techniques and Methods 6-A37, 44 p., available at \url{https://pubs.usgs.gov/tm/tm6a37/}.
#'
#'   Panday, Sorab, Langevin, C.D., Niswonger, R.G., Ibaraki, Motomu, and Hughes, J.D., 2013, MODFLOW-USG version 1:
#'   An unstructured grid version of MODFLOW for simulating groundwater flow and tightly coupled processes using a
#'   control volume finite-difference formulation: U.S. Geological Survey Techniques and Methods, book 6, chap. A45,
#'   66 p., available at \url{https://pubs.usgs.gov/tm/06/a45/}.
#'
#' @keywords IO
#'
#' @export
#'
#' @examples
#' \dontrun{# see Appendix D. Uncalibrated Groundwater-Flow Model}
#'

WriteModflowInput <- function(rs.model, rech, well, trib, misc, river, drain,
                              id, dir.run, is.convertible=FALSE, ss.perlen=0L,
                              tr.stress.periods=NULL, ntime.steps=4L,
                              mv.flag=1e+09, auto.flow.reduce=FALSE,
                              verbose=TRUE) {

  dir.create(path=dir.run, showWarnings=FALSE, recursive=TRUE)
  header <- paste0("# Wood River Valley flow model (", Sys.time(), " ",
                   Sys.timezone(), ")")
  iprn <- if (verbose) 3 else -1  # flag for writing to listing file

  # Stress periods

  is.transient <- inherits(tr.stress.periods, "Date")

  perlen <- as.integer(ss.perlen)  # stress period length
  nstp   <- 1L    # number of time steps in a stress period
  ss.tr  <- "SS"  # steady-state or transient stress period
  yr.mo  <- "ss"  # stress period identifier
  if (is.transient) {
    perlen <- c(perlen, as.integer(diff(tr.stress.periods)))
    nstp   <- c(nstp, rep(as.integer(ntime.steps), length(tr.stress.periods) - 1L))
    ss.tr  <- c(ss.tr, rep("TR", length(perlen) - 1L))
    yr.mo  <- c(yr.mo, format(utils::head(tr.stress.periods, -1), "%Y%m"))
  }

  # Unique unit numbers for output files

  nunit.bud <- 50L  # flow budget
  nunit.hds <- 51L  # hydraulic heads
  nunit.txt <- 52L  # auto flow reduce

  # List file (LST)

  f <- file.path(dir.run, paste0(id, ".lst"))
  nunit <- 10L
  nam <- data.frame(ftype="LIST", nunit=nunit, fname=basename(f))

  # Basic file (BAS6)

  f <- file.path(dir.run, paste0(id, ".ba6"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="BAS6", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW Basic Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  ds.1 <- "PRINTTIME FREE"
  cat(ds.1, file=f, sep="\n", append=TRUE)

  for (i in 1:3) {
    ds.2 <- paste("INTERNAL 1 (FREE)", iprn, " # IBOUND layer", i)
    cat(ds.2, file=f, sep="\n", append=TRUE)
    r <- !is.na(rs.model[[paste0("lay", i, ".bot")]])
    r[] <- as.integer(r[])
    m <- format(raster::as.matrix(r), justify="right", width=2)
    utils::write.table(m, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                       col.names=FALSE)
  }

  hnoflo <- 9999
  ds.3 <- paste(hnoflo, "HNOFLO")
  cat(ds.3, file=f, sep="\n", append=TRUE)

  for (i in 1:3) {
    ds.4 <- paste("INTERNAL 1 (FREE)", iprn, " # STRT layer", i)
    cat(ds.4, file=f, sep="\n", append=TRUE)
    m <- raster::as.matrix(rs.model[[paste0("lay", i, ".strt")]])
    m[is.na(m)] <- hnoflo
    utils::write.table(m, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                       col.names=FALSE)
  }

  # Discretization file (DIS)

  f <- file.path(dir.run, paste0(id, ".dis"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="DIS", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW Discretization Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  ds.1 <- paste(3L, nrow(rs.model), ncol(rs.model), length(perlen), 4L, 2L,
                " # NLAY,NROW,NCOL,NPER,ITMUNI,LENUNI")
  cat(ds.1, file=f, sep="\n", append=TRUE)

  ds.2 <- paste(0, 0, 0, " # LAYBC(NLAY)")
  cat(ds.2, file=f, sep="\n", append=TRUE)

  ds.3 <- paste("CONSTANT", res(rs.model)[2], " # DELR")
  cat(ds.3, file=f, sep="\n", append=TRUE)

  ds.4 <- paste("CONSTANT", res(rs.model)[1], " # DELC")
  cat(ds.4, file=f, sep="\n", append=TRUE)

  ds.5 <- paste("INTERNAL 1 (FREE)", iprn, " # TOP layer 1")
  cat(ds.5, file=f, sep="\n", append=TRUE)

  m <- raster::as.matrix(rs.model[["lay1.top"]])
  m[is.na(m)] <- 0.0
  utils::write.table(m, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                     col.names=FALSE)

  for (i in 1:3) {
    ds.6 <- paste("INTERNAL 1 (FREE)", iprn, " # BOTM layer", i)
    cat(ds.6, file=f, sep="\n", append=TRUE)
    m <- raster::as.matrix(rs.model[[paste0("lay", i, ".bot")]])
    m[is.na(m)] <- 0.0
    utils::write.table(m, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                       col.names=FALSE)
  }

  ds.7 <- data.frame(perlen=perlen, nstp=nstp, tsmult=1, ss.tr=ss.tr)
  utils::write.table(ds.7, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                     col.names=FALSE)

  # Layer-Property Flow file (LPF)

  f <- file.path(dir.run, paste0(id, ".lpf"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="LPF", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW Layer-Property Flow Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  ilpfcb <- if (verbose) nunit.bud else 0
  ds.1 <- paste(ilpfcb, -1e+30, 1, "CONSTANTCV", "NOVFC")
  if (!is.convertible) ds.1 <- paste(ds.1, "STORAGECOEFFICIENT")
  ds.1 <- paste(ds.1, " # ILPFCB,HDRY,NPLPF,[Options]")
  cat(ds.1, file=f, sep="\n", append=TRUE)
  laytyp <- if (is.convertible) c(4, 4, 4) else c(0, 0, 0)
  ds.2 <- paste(laytyp[1], laytyp[2], laytyp[3], " # LAYTYP")
  cat(ds.2, file=f, sep="\n", append=TRUE)
  ds.3 <- paste(0, 0, 0, " # LAYAVG")
  cat(ds.3, file=f, sep="\n", append=TRUE)
  ds.4 <- paste(1, 1, 1, " # CHANI")
  cat(ds.4, file=f, sep="\n", append=TRUE)
  ds.5 <- paste(1, 1, 1, " # LAYVKA")
  cat(ds.5, file=f, sep="\n", append=TRUE)
  ds.6 <- paste(0, 0, 0, " # LAYWET")
  cat(ds.6, file=f, sep="\n", append=TRUE)

  vani <- levels(rs.model[["lay1.zones"]])[[1]]$vani  # TODO(jcf): apply to zone
  ds.8 <- paste("VERTANISO VANI", vani, "3")
  cat(ds.8, file=f, sep="\n", append=TRUE)

  for (i in 1:3) {
    ds.9 <- paste(i, "NONE", "ALL")
    cat(ds.9, file=f, sep="\n", append=TRUE)
  }

  for (i in 1:3) {
    z <- paste0("lay", i, ".zones")
    f.ref <- paste0("hk", i, ".ref")
    fmt <- paste0("OPEN/CLOSE '", f.ref, "' 1.0 '(FREE)' ", iprn, " hk", i)
    cat(fmt, file=f, sep="\n", append=TRUE)
    cat(ds.13 <- 0, file=f, sep="\n", append=TRUE)
    r <- rs.model[[paste0("lay", i, ".hk")]]
    r[is.na(r)] <- mv.flag
    utils::write.table(raster::as.matrix(r), file=file.path(dir.run, f.ref),
                       quote=FALSE, row.names=FALSE, col.names=FALSE)

    if (!is.transient) next
    f.ref <- paste0("ss", i, ".ref")
    fmt <- paste0("OPEN/CLOSE '", f.ref, "' 1.0 '(FREE)' ", iprn, " ss", i)
    cat(fmt, file=f, sep="\n", append=TRUE)
    r <- deratify(rs.model[[z]], ifelse(is.convertible, "ss", "sc"))
    r[is.na(r)] <- mv.flag
    utils::write.table(raster::as.matrix(r), file=file.path(dir.run, f.ref),
                       quote=FALSE, row.names=FALSE, col.names=FALSE)

    if (!is.convertible) next
    f.ref <- paste0("sy", i, ".ref")
    fmt <- paste0("OPEN/CLOSE '", f.ref, "' 1.0 '(FREE)' ", iprn, " sy", i)
    cat(fmt, file=f, sep="\n", append=TRUE)
    r <- deratify(rs.model[[z]], "sy")
    r[is.na(r)] <- mv.flag
    utils::write.table(raster::as.matrix(r), file=file.path(dir.run, f.ref),
                       quote=FALSE, row.names=FALSE, col.names=FALSE)
  }

  # Drain file (DRN)

  f <- file.path(dir.run, paste0(id, ".drn"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="DRN", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW Drain Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  d <- drain[, c("lay", "row", "col", "elev", "cond", "id"), drop=FALSE]

  ds.2 <- paste(nrow(d), nunit.bud, "AUXILIARY id")
  if (!verbose)
    ds.2 <- paste(ds.2, "NOPRINT")
  ds.2 <- paste(ds.2, " # MXACTD,IDRNCB,[Option]")
  cat(ds.2, file=f, sep="\n", append=TRUE)
  ds.5 <- paste(nrow(d), 0, " # ITMP,NP")
  cat(ds.5, file=f, sep="\n", append=TRUE)
  utils::write.table(d, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                     col.names=FALSE)
  if (is.transient) {
    ds.5 <- paste(-1, 0, " # ITMP,NP")
    for (i in 2:length(perlen)) {
      cat(ds.5, file=f, sep="\n", append=TRUE)
    }
  }

  # River file (RIV)

  f <- file.path(dir.run, paste0(id, ".riv"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="RIV", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW River Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  n <- nrow(river)
  ds.2 <- paste(n, nunit.bud, "AUXILIARY id")
  if (!verbose)
    ds.2 <- paste(ds.2, "NOPRINT")
  ds.2 <- paste(ds.2, " # MXACTR,IRIVCB,[Option]")
  cat(ds.2, file=f, sep="\n", append=TRUE)

  for (i in yr.mo) {
    ds.5 <- paste(n, 0L, " # ITMP,NP    STRESS PERIOD", i)
    cat(ds.5, file=f, sep="\n", append=TRUE)
    d <- river[, c("lay", "row", "col", i, "cond", "bottom", "id")]
    utils::write.table(d, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                       col.names=FALSE)
  }

  # Well file (WEL)

  f <- file.path(dir.run, paste0(id, ".wel"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="WEL", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW Well Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  rech <- as.matrix(cbind(rech[, c("lay", "row", "col", yr.mo)], id=1))
  well <- as.matrix(cbind(well[, c("lay", "row", "col", yr.mo)], id=2))
  trib <- as.matrix(cbind(trib[, c("lay", "row", "col", yr.mo)], id=3))
  misc <- as.matrix(cbind(misc[, c("lay", "row", "col", yr.mo)], id=4))
  m <- rbind(rech, well, trib, misc)

  ds.2 <- paste(nrow(m), nunit.bud, "AUXILIARY id")
  if (!verbose)
    ds.2 <- paste(ds.2, "NOPRINT")
  if (auto.flow.reduce)
    ds.2 <- paste(ds.2, "AUTOFLOWREDUCE IUNITAFR", nunit.txt)
  ds.2 <- paste(ds.2, " # MXACTW,IWELCB,[Option]")
  cat(ds.2, file=f, sep="\n", append=TRUE)

  for (i in yr.mo) {
    mm <- m[, c("lay", "row", "col", i, "id")]
    mm <- mm[mm[, i] != 0, , drop=FALSE]
    if (nrow(mm) == 0) next
    ds.5 <- paste(nrow(mm), 0, " # ITMP,NP    STRESS PERIOD", i)
    cat(ds.5, file=f, sep="\n", append=TRUE)
    utils::write.table(mm, file=f, append=TRUE, quote=FALSE, row.names=FALSE,
                       col.names=FALSE)
  }

  # Sparse Matrix Solver file (SMS)

  f <- file.path(dir.run, paste0(id, ".sms"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="SMS", nunit=nunit, fname=basename(f)))
  ds.0 <- c(header, "# MODFLOW Sparse Matrix Solver Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)
  iprsms <- if (verbose) 1 else 0
  if (is.convertible) {
    cat("COMPLEX OPTIONS", file=f, sep="\n", append=TRUE)
    ds <- paste(0.0001, 0.001, 2000, 500, iprsms, 1, 1,
                " # HCLOSE,HICLOSE,MXITER,ITER1,IPRSMS,NONLINMETH,LINMETH")
    cat(ds, file=f, sep="\n", append=TRUE)
  } else {
    ds <- paste(0.001, 0.01, 1000, 500, iprsms, 1, 1,
                " # HCLOSE,HICLOSE,MXITER,ITER1,IPRSMS,NONLINMETH,LINMETH")
    cat(ds, file=f, sep="\n", append=TRUE)
    ds <- paste(0.9, 0.0001, 0, 0, 10, 10000, 0.2, 100,
                " # THETA,AKAPPA,GAMMA,AMOMENTUM,NUMTRACK,BTOL,BREDUC,PESLIM")
    cat(ds, file=f, sep="\n", append=TRUE)
    ds <- paste(0, 0, 3, 5, 0, 0, 1, 0.001,
                " # IACL,NORDER,LEVEL,NORTH,IREDSYS,RRCTOL,IDROPTOL,EPSRN")
    cat(ds, file=f, sep="\n", append=TRUE)
  }

  # Output control (OC)

  f <- file.path(dir.run, paste0(id, ".oc"))
  nunit <- nunit + 1L
  nam <- rbind(nam, data.frame(ftype="OC", nunit=nunit, fname=basename(f)))

  ds.0 <- c(header, "# MODFLOW Output Control Package")
  cat(ds.0, file=f, sep="\n", append=FALSE)

  ds.1 <- c(paste("HEAD SAVE UNIT", nunit.hds), "COMPACT BUDGET AUXILIARY")
  cat(ds.1, file=f, sep="\n", append=TRUE)

  for (i in seq_along(yr.mo)) {
    for (j in seq_len(nstp[i])) {
      ds.2 <- paste("\nPERIOD", i, "STEP", j)
      cat(ds.2, file=f, sep="\n", append=TRUE)
      ds.3 <- paste("    ", c("SAVE HEAD", "SAVE BUDGET"))
      cat(ds.3, file=f, sep="\n", append=TRUE)
    }
  }

  # Name file

  nam <- rbind(nam, data.frame(ftype="DATA(BINARY)", nunit=nunit.bud,
                               fname=paste0(id, ".bud")))
  nam <- rbind(nam, data.frame(ftype="DATA(BINARY)", nunit=nunit.hds,
                               fname=paste0(id, ".hds")))
  if (auto.flow.reduce)
    nam <- rbind(nam, data.frame(ftype="DATA", nunit=nunit.txt,
                                 fname=paste0(id, ".afr")))
  f <- file.path(dir.run, paste0(id, ".nam"))
  utils::write.table(nam, file=f, append=FALSE, quote=FALSE, sep=" ",
                     row.names=FALSE, col.names=FALSE)

  invisible(NULL)
}
