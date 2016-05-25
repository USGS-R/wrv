\name{ReadModflowBinary}

\alias{ReadModflowBinary}

\title{Read MODFLOW Binary File}

\description{
This function reads binary output data from a \href{http://water.usgs.gov/ogw/modflow/}{MODFLOW} run.
}

\usage{
ReadModflowBinary(f, data.type = c("array", "flow"), rm.totim.0 = FALSE)
}

\arguments{
\item{f}{\code{character}; the name of the binary file.}
\item{data.type}{\code{character}; a description of how the data are saved.}
\item{rm.totim.0}{\code{logical}; if \code{TRUE}, stress-periods at time zero are removed.}
}

\details{
This function reads binary head (\file{.hds}), drawdown (\file{.ddn}), and budget (\file{.bud}) files generated from a MODFLOW run.
}

\value{
Returns a \code{list} object of length equal to the number of times the data type is written to the binary file.
List components are \code{list} objects with the following components:
\item{d}{\code{matrix} or \code{data.frame}; the data values.}
\item{kstp}{\code{integer}; the time step.}
\item{kper}{\code{integer}; the stress period.}
\item{desc}{\code{character}; the variable name.}
\item{ilay}{\code{integer}; the model-grid layer.}
\item{delt}{\code{numeric}; the length of the current time step.}
\item{pertim}{\code{numeric}; the time in the stress period.}
\item{totim}{\code{numeric}; the total elapsed time.}
}

\author{J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center}

\seealso{\code{\link{SummariseBudget}}}

\examples{\dontrun{
path <- file.path(getwd(), "SIR2016-XXXX/output/output.model1")
hds <- ReadModflowBinary(file.path(path, "wrv_mfusg.hds"), "array")
bud <- ReadModflowBinary(file.path(path, "wrv_mfusg.bud"), "flow")}
}

\keyword{IO}
