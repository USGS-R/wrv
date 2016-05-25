\name{WriteModflowInput}

\alias{WriteModflowInput}

\title{Write MODFLOW Input Files}

\description{
This function generates and writes input files for a MODFLOW simulation of groundwater flow in the Wood River Valley (WRV) aquifer system.
}

\usage{
WriteModflowInput(rs.model, rech, well, trib, misc, river, drain, id, dir.run,
                  is.convertible = FALSE, ss.perlen = 0L,
                  tr.stress.periods = NULL, ntime.steps = 4L,
                  mv.flag = 1e+09, auto.flow.reduce = FALSE, verbose = TRUE)
}

\arguments{
\item{rs.model}{\code{RasterStack}; a collection of \code{RasterLayer} objects with the same extent and resolution, see \sQuote{Details} for required raster layers.}
\item{rech}{\code{data.frame}; is the groundwater flow across the water table, in cubic meters per day.
  Variables describe the model cell location (\code{lay}, \code{row}, \code{col}) and volumetric rate during each stress period (\code{ss}, \code{199501}, \code{199502}, \dots, \code{201012}).}
\item{well}{\code{data.frame}; is the well pumping at point locations in cubic meters per day.
  Variables describe the model cell location and volumetric rate during each stress period.}
\item{trib}{\code{data.frame}; is the incoming flows from the major tributary canyons.
  Variables describe the model cell location and volumetric rate during each stress period.}
\item{misc}{\code{data.frame}; is recharge from miscellaneous seepage sites in cubic meters per day.
  Variables describe the model cell location and volumetric rate during each stress period.}
\item{river}{\code{data.frame}; is the river conditions.
  Variables describe the model cell location, river conductance (\code{cond}) in square meters per day, river bottom elevation (\code{bottom}) in meters above the North American Vertical Datum of 1988 (NAVD 88), and a numeric river reach identifier (\code{id}).}
\item{drain}{\code{data.frame}; is the drain conditions for groundwater outlet boundaries.
  Variables describe the model cell location, drain threshold elevation (\code{elev}) in meters above the NAVD 88, drain conductance (\code{cond}) in square meters per day, and a numeric identifier (\code{id}) indicating the drains general location.}
\item{id}{\code{character}; a short identifier for the model run.}
\item{dir.run}{\code{character}; the path name of the directory to write model input files.}
\item{is.convertible}{\code{logical}; if \code{TRUE}, indicates model layers are \sQuote{convertible}, with transmissivity computed using upstream water-table depth.
  Otherwise, model layers are \sQuote{confined} and transmissivity is constant over time.}
\item{ss.perlen}{\code{integer} or \code{difftime}; the length of the steady-state stress period in days.}
\item{tr.stress.periods}{\code{Date}; a vector of start times for each stress period in the transient simulation.
  If missing, only steady-state conditions are simulated.}
\item{ntime.steps}{\code{integer}; the number of uniform time steps in a stress period.}
\item{mv.flag}{\code{numeric}; default \code{NA}, missing value flag for output reference data files.}
\item{auto.flow.reduce}{\code{logical}; if \code{TRUE}, a simulated well will adjust pumping according to supply under bottom-hole conditions.
  Pumping rates that have been automatically reduced will be written to a model output file (\file{.afr}).}
\item{verbose}{\code{logical}; if \code{TRUE}, additional information is written to the listing file (\file{.lst}) and budget file (\file{.bud}).}
}

\details{
Groundwater flow in the WRV aquifer system is simulated using the \href{http://water.usgs.gov/ogw/mfusg/}{MODFLOW-USG} groundwater-flow model.
This numerical model was chosen for its ability to solve complex unconfined groundwater flow simulations.
The solver implemented in MODFLOW-USG incorporates the Newton-Raphson formulation for improving solution convergence and avoiding problems with the drying and rewetting of cells (Niswonger and others, 2011).
A structured finite-difference grid is implemented in the model to
(1) simplify discretization,
(2) keep formats and structures for the MODFLOW-USG packages identical to those of \href{http://water.usgs.gov/nrp/gwsoftware/modflow2005/Guide/index.html}{MODFLOW-2005}, and
(3) allow any MODFLOW post-processor to be used to analyze the results of the MODFLOW-USG simulation (such as \href{http://water.usgs.gov/nrp/gwsoftware/modelviewer/ModelViewer.html}{Model Viewer}).

Model input files are written to \code{dir.run} and include the following MODFLOW Package files: Name (\file{.nam}), Basic (\file{.ba6}), Discretization (\file{.dis}), Layer-Property Flow (\file{.lpf}), Drain (\file{.drn}), River (\file{.riv}),  Well (\file{.wel}), Sparse Matrix Solver (\file{.sms}), and Output Control (\file{.oc}).
See the users guide (\cite{Description of Model Input and Output}) included with the MODFLOW-USG \href{http://water.usgs.gov/ogw/mfusg/mfusg.1_2_00.zip}{software} for details on input file formats and structures.

Data within the \code{rech}, \code{well}, \code{trib}, and \code{misc} arguments are combined in the MODFLOW Well Package and identifiable with added \code{id} values of 1, 2, 3, and 4, respectively.

The Layer-Property Flow file includes options for the calculation of vertical flow in partially dewatered cells.
For the WRV model, where there is no indication that perched conditions exist, \code{CONSTANTCV} and \code{NOVFC} options are used to create the most stable solution (Panday and others, 2013, p. 15-16).
Options for the Sparse Matrix Solver were set for unconfined simulations by implementing an upstream-weighting scheme with Newton-Raphson linearization, Delta-Bar-Delta under-relaxation, and the \eqn{\chi}MD solver of Ibaraki (2005).

The raster stack \code{rs.model} includes the following layers:
\describe{
  \item{lay1.top}{is the elevation at the top of model layer 1 (land surface), in meters above the NAVD 88.}
  \item{lay1.bot}{is the elevation at the bottom of model layer 1, in meters above the NAVD 88.}
  \item{lay2.bot}{is the elevation at the bottom of model layer 2.}
  \item{lay3.bot}{is the elevation at the bottom of model layer 3.}
  \item{lay1.strt}{is the initial (starting) hydraulic head in model layer 1, in meters above the NAVD 88.}
  \item{lay2.strt}{is the initial hydraulic head in model layer 2.}
  \item{lay3.strt}{is the initial hydraulic head in model layer 3.}
  \item{lay1.zones}{is the hydrogeologic zones in model layer 1 where values \code{= 1} is unconfined alluvium, \code{= 2} is basalt, \code{= 3} is clay, and \code{= 4} is confined alluvium.}
  \item{lay2.zones}{is the hydrogeologic zones in model layer 2.}
  \item{lay3.zones}{is the hydrogeologic zones in model layer 3.}
  \item{lay1.hk}{is the horizontal hydraulic conductivity in model layer 1, in meters per day.}
  \item{lay2.hk}{is the horizontal hydraulic conductivity in model layer 2.}
  \item{lay3.hk}{is the horizontal hydraulic conductivity in model layer 3.}
}
}

\value{None. Used for the side-effect of files written to disk.}

\references{
Ibaraki, M., 2005, \eqn{\chi}MD User's guide-An efficient sparse matrix solver library, version 1.30: Columbus, Ohio State University School of Earth Sciences.

Niswonger, R.G., Panday, Sorab, and Ibaraki, Motomu, 2011, MODFLOW-NWT, A Newton formulation for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A37, 44 p., available at \url{http://pubs.usgs.gov/tm/tm6a37/}.

Panday, Sorab, Langevin, C.D., Niswonger, R.G., Ibaraki, Motomu, and Hughes, J.D., 2013, MODFLOW-USG version 1: An unstructured grid version of MODFLOW for simulating groundwater flow and tightly coupled processes using a control volume finite-difference formulation: U.S. Geological Survey Techniques and Methods, book 6, chap. A45, 66 p., available at \url{http://pubs.usgs.gov/tm/06/a45/}.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\examples{\dontrun{# see uncalibrated-model vignette}}

\keyword{IO}
