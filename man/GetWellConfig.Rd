\name{GetWellConfig}

\alias{GetWellConfig}

\title{Get Well Completion and Pumping Rate in Model Space}

\description{
This function determines well completions and pumping rates in model space.
The pumping rate is specified for each model cell intersecting a well's open interval(s)
and calculated by multiplying the estimated pumping demand by the cell's transmissivity fraction.
The transmissivity fraction is calculated by dividing the cell's aquifer transmissivity by the sum of all transmissivity values for cells belonging to the same well.
The transmissivity fraction calculation assumes saturated conditions in the model cell.
}

\usage{
GetWellConfig(rs.model, wells, well.col, rate.col = NULL, lay2.hk.tol = 1e-02)
}

\arguments{
\item{rs.model}{\code{RasterStack}; is composed of raster layers describing the model grid and hydraulic conductivity distribution:
  \code{lay1.top}, \code{lay1.bot}, \code{lay2.bot}, \code{lay3.bot}, \code{lay1.top}, \code{lay1.hk}, \code{lay2.hk}, and \code{lay3.hk}.}
\item{wells}{\code{SpatialPointsDataFrame}; is the average pumping rate for each well during various times.}
\item{well.col}{\code{character}; is the column name of the well identifier field.}
\item{rate.col}{\code{character}; is a vector of column names for the pumping rate fields.}
\item{lay2.hk.tol}{\code{numeric}; is the hydraulic conductivity tolerance for model cells in layer 2.
  Used to prevent pumping in the aquitard layer of the aquifer system.
  Pumping is prohibited in model layer 2 cells with hydraulic conductivity values less than \code{lay2.hk.tol} and a well opening isolated to layer 2;
  for these cases, pumping is allocated to the adjacent layer 1 cell.}
}

\value{
An object of \code{data.frame} class with the following components:
\item{\dots}{\code{numeric}; a unique identifier assigned to a well, its name is specified by \code{well.col}.}
\item{lay, row, col}{\code{integer}; is the layer, row, and column number of a model cell, respectively.}
\item{hk}{\code{numeric}; is the hydraulic conductivity of the model cell, in meters per day.}
\item{thk}{\code{numeric}; is the vertical length of the well opening (open borehole or screen) in the model cell, in meters.
  A value of zero indicates that the well opening is unknown or below the modeled bedrock surface.}
\item{frac}{\code{numeric}; is the transmissivity fraction for a model cell, where transmissivity is defined as \code{hk} multiplied by \code{thk}.}
\item{\dots}{\code{numeric}; is the pumping rate allocated to the model cell for each time period specified by \code{rate.col}, in cubic meters per day.
  The pumping rate is calculated by multiplying the pumping demand for a well (specified in \code{wells}) by \code{frac}.}
}

\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center

A.H. Wylie, Idaho Department of Water Resources
}

\examples{\dontrun{# see uncalibrated-model vignette}}

\keyword{manip}
