\name{SummariseBudget}

\alias{SummariseBudget}

\title{Summarize Volumetric Water Budget}

\description{
Summarizes volumetric flow rates for boundary condition types.
Splits the budget data into subsets, computes summary statistics for each, and returns the result in a summary table.
}

\usage{
SummariseBudget(budget, desc = c("wells", "drains", "river leakage"))
}

\arguments{
\item{budget}{\code{character} or \code{list}; either a description of the path to the MODFLOW Budget File or the returned results from a call to the \code{\link{ReadModflowBinary}} function.}
\item{desc}{\code{character}; a vector of MODFLOW package identifiers.
  Data of this package type is included in the summary table.}
}

\details{
The \code{budget[[i]]$d} data table component must contain a numeric \code{id} field, see \code{\link{WriteModflowInput}} for variable description.
Subsets are grouped by the MODFLOW package identifier (\code{desc}), stress period (\code{kper}), time step (\code{kstp}), and location identifier (\code{id}).
}

\value{
Returns an object of \code{data.frame} class with the following components:
\item{desc}{\code{factor}; is the MODFLOW package identifier.}
\item{kper}{\code{integer}; is the stress period.}
\item{kstp}{\code{integer}; is the time step.}
\item{id}{\code{integer}; is a location identifier.}
\item{delt}{\code{numeric}; is the length of the current time step.}
\item{pertim}{\code{numeric}; is the time in the stress period.}
\item{totim}{\code{numeric}; is the total elapsed time.}
\item{count}{\code{integer}; is the number of cells in each subset.}
\item{flow.sum}{\code{numeric}; is the total volumetric flow rate.}
\item{flow.mean}{\code{numeric}; is the mean volumetric flow rate.}
\item{flow.median}{\code{numeric}; is the median volumetric flow rate.}
\item{flow.sd}{\code{numeric}; is the standard deviation of the volumetric flow rate.}
\item{flow.dir}{\code{factor}; is the flow direction where \code{"in"} and \code{"out"} indicate water entering and leaving the groundwater system, respectively.}
}

\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}

\examples{\dontrun{
f <- file.path(getwd(), "SIR2016-5080/output/output.model1/wrv_mfusg.bud")
d <- SummariseBudget(f)
str(d)}
}

\keyword{utilities}
