\name{UpdateWaterBudget}

\alias{UpdateWaterBudget}

\title{Update Water Budget}

\description{
This function runs the water budget calculation and updates the MODFLOW Well Package file.
It is executed during each iteration of PEST and my be run in an interactive \R session to initialize the parameter estimation files.
}

\usage{
UpdateWaterBudget(dir.run, id, qa.tables = c("none", "si", "english"),
                  ss.interval = NULL, iwelcb = 0L)
}

\arguments{
\item{dir.run}{\code{character}; the path name of the directory to read/write model files.}
\item{id}{\code{character}; a short identifier (file name) for model files.}
\item{qa.tables}{\code{character}; indicates if quality assurance tables are written to disk; by default \code{"none"} of these tables are written.
   Values of \code{"si"} and \code{"english"} indicate that table values are written in metric and English units, respectively.}
\item{ss.interval}{\code{Date} or \code{character}; a vector of length 2 specifying the start and end dates for the period used to represent steady-state boundary conditions.
  That is, recharge values for stress periods coinciding with this time period are averaged and used as steady-state boundary conditions.
  The required date format is \code{YYYY-MM-DD}.
  This argument overrides the \code{ss.stress.periods} object in the \file{model.rda} file, see \sQuote{Details} section for additional information.}
\item{iwelcb}{\code{integer}; is a flag and unit number.
  If equal to zero, the default, cell-by-cell flow terms resulting from conditions in the MODFLOW Well Package will not be written to disk.
  This default value is appropriate for model calibration, where MODFLOW run times are kept as short as possible.
  If greater than zero, the cell-by-cell flow terms are written to disk.
  See the MODFLOW Name File (\file{*.nam}) for the unit number associated with the budget file (\file{*.bud}).
  }
}

\details{
Files read during execution, and located within the \code{dir.run} directory, include
the MODFLOW hydraulic conductivity reference files \file{hk1.ref}, \file{hk2.ref}, and \file{hk3.ref}
corresponding to model layers 1, 2, and 3, respectively.
Hydraulic conductivity values are read from a two-dimensional array in matrix format with \sQuote{white-space} delimited fields.
And a binary data file \file{model.rda} containing the following serialized \R objects:
\code{rs}, \code{misc}, \code{trib}, \code{tr.stress.periods}, and \code{ss.stress.periods}.

\code{rs} is an object of \code{RasterStack} class with raster layers
\dQuote{lay1.top}, \dQuote{lay1.bot}, \dQuote{lay2.bot}, and \dQuote{lay3.bot}.
These raster layers describe the geometry of the model grid; that is,
the upper and lower elevation of model layer 1, and the bottom elevations of model layers 2 and 3.
In addition to these layers, \code{rs} includes ancillary raster layers \dQuote{lay1.zones}, \dQuote{lay2.zones}, and \dQuote{lay3.zones}
describing the distribution of hydrogeologic zones in the model grid.
Missing cell values (equal to \code{NA}) indicate inactive model cells lying outside of the model domain.

\code{misc} is a \code{data.frame} object with miscellaneous seepage,
such as from the \sQuote{Bellevue Waste Water Treatment Plant ponds} and the \sQuote{Bypass Canal}.
This object is comprised of the following components:
\bold{lay}, \bold{row}, \bold{col} are \code{integer} values specifying a model cell's layer, row, and column index, respectively; and
\bold{ss}, \bold{199501}, \bold{199502}, \dots, \bold{201012} are numeric values of elevation during each stress period, respectively,
in meters above the North American Vertical Datum of 1988.

\code{trib} is a \code{data.frame} object with default values for the long-term mean underflows in each of the tributary basins.
The object is comprised of the following components:
\bold{Name} is a unique identifier for the tributary basin;
\bold{lay}, \bold{row}, \bold{col} are \code{integer} values of a model cell's layer, row, and column index, respectively; and
\bold{ss}, \bold{199501}, \bold{199502}, \dots, \bold{201012} are numeric values of underflow during each stress period, respectively,
in cubic meters per day.

\code{tr.stress.periods} is a vector of \code{Date} values giving the start and end dates for stress periods in the model simulation period (1995--2010).

\code{ss.stress.periods} is a vector of \code{Date} values giving the start and end dates for stress periods used to define steady-state conditions.

\code{reduction} is a \code{numeric} default value for the signal amplitude reduction algorithm, a dimensionless quantity.

\code{d.in.mv.ave} is a \code{numeric} default value for the number of days in the moving average subset.
}

\value{
Returns an object of \code{difftime} class, the runtime for this function.
Used for the side-effect of files written to disk.

A MODFLOW Well Package file \file{<id>.wel} is always written to disk; whereas,
parameter estimation files \file{seep.csv}, \file{eff.csv}, and \file{trib.csv}, and
a script file \file{UpdateBudget.bat}, are only written if they do not already exist.
The script file may be used to automate the execution of this function from a file manager (such as, Windows Explorer).

The \file{seep.csv} file stores as tabular data the canal seepage fraction for each of the irrigation entities.
Its \code{character} and \code{numeric} data fields are delimited by commas (a comma-separated-value [CSV] file).
The first line is reserved for field names \code{EntityName} and \code{SeepFrac}.

The \file{eff.csv} file stores as tabular data the irrigation efficiency for each of the irrigation entities.
Its \code{character} and \code{numeric} data fields are delimited by commas.
The first line is reserved for field names \code{EntityName} and \code{Eff}.

The \file{trib.csv} file stores as tabular data the underflow boundary conditions for each tributary basin.
Its \code{character} and \code{numeric} data fields are delimited by commas.
The first line is reserved for field names \code{Name} and \code{Value}.
Data records include a long-term mean flow multiplier for each of the tributary basins (name is the unique identifier for the tributary),
a record for the amplitude reduction (\code{reduction}), and
a record for the number of days in the moving average (\code{d.in.mv.ave}).

If the \code{qa.tables} argument is specified as either \code{"si"} or \code{"english"}, quality assurance tables are written to disk as CSV files (\sQuote{qa-*.csv}).
Volumetric flow rate data within these tables is described in the \sQuote{Value} section of the \code{\link{RunWaterBalance}} function;
see returned \code{list} components \code{natural.rech}, \code{inciden.rech}, and \code{pumping.rech}.
The well configuration data are described in the \sQuote{Value} section of the \code{\link{GetWellConfig}} function.
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{\link{RunWaterBalance}}, \code{\link{GetSeasonalMult}}}

\examples{\dontrun{
dir.run <- "C:/Users/jfisher/Documents/SIR2016-XXXX/model/model1"
UpdateWaterBudget(dir.run, "wrv_mfusg", qa.tables = "si",
                  ss.interval = c("1998-01-01", "2011-01-01"))}
}

\keyword{utilities}
