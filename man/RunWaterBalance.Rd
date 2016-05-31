\name{RunWaterBalance}

\alias{RunWaterBalance}

\title{Run Water Balance}

\description{
This function estimates areal recharge, and pumping demand at production wells.
A water-balance approach is used to calculate these volumetric flow rate estimates,
where positive values are flow into the aqufer system (groundwater recharge),
and negative values are flow out of the system (groundwater discharge).
}

\usage{
RunWaterBalance(tr.stress.periods, r.grid, eff, seep, ss.stress.periods = NULL,
                verbose = FALSE)
}

\arguments{
\item{tr.stress.periods}{\code{Date}; a vector of start and end dates for each stress period in the simulation.}
\item{r.grid}{\code{RasterLayer}; a raster of numeric values where \code{NA} indicates an \sQuote{inactive} cell in the top layer of the model.}
\item{eff}{\code{data.frame}; see \code{\link{efficiency}} dataset for details.}
\item{seep}{\code{data.frame}; see \code{\link{canal.seep}} dataset for details.}
\item{ss.stress.periods}{\code{Date}; a vector of start and end dates for stress periods used to create steady-state conditions.}
\item{verbose}{\code{logical}; indicates whether to return summary tables \code{natural.rech}, \code{inciden.rech}, \code{pumping.rech};
  see \sQuote{Value} section for table formats.}
}

\value{
Returns a \code{list} object with the following components:

(1) Water-table flow data (combines natural and incidental groundwater recharge and discharge) are stored in \code{areal.rech},
an object of \code{RasterStack} class with raster layers for each model stress period;
cell values are specified as volumetric flow rates in cubic meters per day.

(2) Production well pumping data are stored in \code{pod.rech}, an object of \code{data.frame} class with the following components:
\item{WMISNumber}{\code{numeric}; a unique number assigned to a water right point of diversion.}
\item{ss, 199501, \dots, 201012}{\code{numeric}; is the volumetric flow rate, specified for each stress period, in cubic meters per day.}

(3) Natural groundwater recharge and discharge data are stored in \code{natural.rech}, an object of \code{data.frame} class with the following components:
\item{YearMonth}{\code{factor}; is the calendar year and month \code{YYYYMM}.}
\item{Area}{\code{numeric}; the land-surface area of non-irrigated lands, in square meters.}
\item{ET}{\code{numeric}; evapotranspiration on non-irrigated lands, in cubic meters per month.}
\item{Rech}{\code{numeric}; is the volumetric flow rate, in cubic meters per month.}

(4) Incidental groundwater recharge data are stored in \code{inciden.rech}, an object of \code{data.frame} class with the following components:
\item{EntityName}{\code{character}; is the name of the irrigation entity.}
\item{YearMonth}{\code{factor}; is the calendar year and month \code{YYYYMM}.}
\item{SWDiv}{\code{numeric}; surface-water diversions, in cubic meters per month.}
\item{SeepFrac}{\code{numeric}; canal seepage as a fraction of diversions, a dimensionless quantity.}
\item{CanalSeep}{\code{numeric}; canal seepage, in cubic meters per month.}
\item{SWDel}{\code{numeric}; surface-water delivered to field headgates, in cubic meters per month.}
\item{area.sw}{\code{numeric}; area irrigated by only surface water, in square meters.}
\item{et.sw}{\code{numeric}; evapotranspiration on lands irrigated by only surface water, in cubic meters per month.}
\item{precip.sw}{\code{numeric}; precipitation on lands irrigated by only surface water, in cubic meters per month.}
\item{cir.sw}{\code{numeric}; crop irrigation requirement on lands irrigated by only surface water, in cubic meters per month.}
\item{area.mix}{\code{numeric}; area irrigated by both surface and groundwater, in square meters.}
\item{et.mix}{\code{numeric}; evapotranspiration on lands irrigated by both surface and groundwater, in cubic meters per month.}
\item{precip.mix}{\code{numeric}; precipitation on lands irrigated by both surface and groundwater, in cubic meters per month.}
\item{cir.mix}{\code{numeric}; crop irrigation requirement on lands irrigated by both surface and groundwater, in cubic meters per month.}
\item{area.gw}{\code{numeric}; area irrigated by only groundwater, in square meters.}
\item{et.gw}{\code{numeric}; evapotranspiration on lands irrigated by only groundwater, in cubic meters per month.}
\item{precip.gw}{\code{numeric}; precipitation on lands irrigated by only groundwater, in cubic meters per month.}
\item{cir.gw}{\code{numeric}; crop irrigation requirement on lands irrigated by only groundwater, in cubic meters per month.}
\item{Eff}{\code{numeric}; irrigation efficiency, a dimensionless quantity.}
\item{GWDiv}{\code{numeric}; recorded groundwater diversions, in cubic meters per month.}
\item{WWDiv}{\code{numeric}; inflow to municipal wastewater treatment plants, in cubic meters per month.}
\item{hg.sw}{\code{numeric}; surface-water delivered to field headgates on lands irrigated by only surface water, in cubic meters per month.}
\item{hg.mix}{\code{numeric}; surface-water delivered to field headgates on lands irrigation by both surface and groundwater, in cubic meters per month.}
\item{rech.sw}{\code{numeric}; incidental groundwater recharge beneath lands irrigated by only surface water, in cubic meters per month.}
\item{gw.dem.mix}{\code{numeric}; groundwater demand on lands irrigated by both surface and groundwater, in cubic meters per month.}
\item{gw.div.est}{\code{numeric}; calculated groundwater diversions, in cubic meters per month.}
\item{rech.mix}{\code{numeric}; incidental groundwater recharge beneath lands irrigated by both surface and groundwater, in cubic meters per month.}
\item{gw.only}{\code{numeric}; groundwater demand on lands irrigated by only groundwater in entities with lands also irrigated by both surface and groundwater, in cubic meters per month.}
\item{rech.muni}{\code{numeric}; incidental groundwater recharge beneath entities with lands irrigated by only groundwater and lands irrigated by both surface and groundwater, in cubic meters per month.}
\item{gw.dem.gw}{\code{numeric}; groundwater demand on lands irrigated by only groundwater in entities without surface-water irrigation, in cubic meters per month.}
\item{rech.gw}{\code{numeric}; incidental groundwater recharge beneath lands irrigated by only groundwater, in cubic meters per month.}
\item{area.model}{\code{numeric}; area of the irrigation entity that is located in the model domain, in square meters.}
Volumetric flow rates are calculated for their respective area in the irrigation entity---not just that part overlying the model area.
Flow rate values are given this way in order to facilitate with quality assurance of the water-budget calculation.
To calculate a simulated volumetric-flow rate: divide the flow rate by the affected area, and then multiply this value by the area of the irrigation entity that is located in the model domain (\code{area.model}).

(5) Well pumping data are also stored in \code{pumping.rech} (see \code{pod.rech} component), an object of \code{data.frame} class with the following components:
\item{WMISNumber}{\code{numeric}; a unique number assigned to a water right point of diversion.}
\item{YearMonth}{\code{factor}; is the calendar year and month \code{YYYYMM}.}
\item{Pumping}{\code{numeric}; is the volumetric rate of pumping, in cubic meters per month.}
}

\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center

J. Sukow and M. McVay, Idaho Department of Water Resources
}

\seealso{\code{\link{UpdateWaterBudget}}}

\examples{\dontrun{# see wrv-introduction vignette}}

\keyword{manip}
