#' Get Seasonal Multiplier
#'
#' This function determines the seasonal fraction of the long-term mean value.
#'
#' @param x data.frame.
#'   Time series data (observations) with components of class Date and numeric.
#' @param reduction numeric.
#'   Signal amplitude reduction, a dimensionless quantity.
#'   Its magnitude should be greater than or equal to 1;
#'   where a value of 1 indicates no reduction in the signal amplitude.
#' @param d.in.mv.ave numeric.
#'   Number of days in the moving average subset.
#' @param fixed.dates Date.
#'   Vector of equally spaced dates, these are the fixed locations where the moving average is calculated.
#'   The final date is neglected.
#'
#' @details A simple moving average is first calculated at dates specified in \code{fixed.dates}
#'   using past observational data in \code{x}
#'  (such as the previous 9-months of stage data recorded at a streamgage).
#'   The seasonal average of the moving average is then passed through a signal amplitude reduction algorithm.
#'   The reduced values are then divided by the mean of the seasonal reduced data to give
#'   the seasonal fraction of the mean (also known as the seasonal scaling index).
#'
#' @return Returns an object of class data.frame with the following variables:
#'   \describe{
#'     \item{\code{names(x)[1]}}{start date for each season in \code{fixed.dates}.}
#'     \item{multiplier}{seasonal scaling index}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#'   A.H. Wylie and J. Sukow, Idaho Department of Water Resources
#'
#' @keywords manip
#'
#' @export
#'
#' @examples
#' obs <- dataRetrieval::readNWISdata(sites = "13139510", parameterCd = "00060",
#'                                    startDate = "1992-01-01", endDate = "2011-01-01")
#' obs <- obs[, c("dateTime", "X_00060_00003")]
#' obs[, 1] <- as.Date(obs[, 1])
#'
#' fixed.dates <- seq(as.Date("1995-01-01"), as.Date("2011-01-01"), "1 month")
#' d <- GetSeasonalMult(obs, 2, 273.932, fixed.dates)
#' str(d)
#'

GetSeasonalMult <- function(x, reduction, d.in.mv.ave, fixed.dates) {

  if (!is.data.frame(x) || !inherits(x[, 1], "Date") || !is.numeric(x[, 2]))
    stop("Problem with 'x' argument")
  if (!is.numeric(reduction) || reduction < 0)
    stop("Problem with 'reduction' argument")
  if (!is.numeric(d.in.mv.ave) || d.in.mv.ave <= 0)
    stop("Problem with 'd.in.mv.ave' argument")
  if (!inherits(fixed.dates, "Date"))
    stop("Problem with 'fixed.dates' argument")

  f <- stats::approxfun(x[, 1], x[, 2], rule=2)
  d <- data.frame(utils::head(fixed.dates, -1))
  FUN <- function(i) {
    vol <- stats::integrate(f, i - d.in.mv.ave, i, subdivisions=1000L, rel.tol=0.001)
    return(vol$value / d.in.mv.ave)
  }
  d[, 2] <- vapply(d[, 1], FUN, 0)  # moving average at fixed dates
  d <- .GetQtrlyMean(d)  # seasonal mean
  d[, 2] <- .ReduceAmp(d[, 2], reduction)  # amplitude reduction
  d$multiplier <- d[, 2] / mean(d[, 2])  # seasonal-scaling index
  d <- d[, c(1, 3)]
  colnames(d) <- c(colnames(x)[1], "multiplier")
  rownames(d) <- NULL

  return(d)
}


.GetQtrlyMean <- function(x) {
  if (!is.data.frame(x) || !inherits(x[, 1], "Date") || !is.numeric(x[, 2]))
    stop("Problem with 'x' argument")
  d <- data.frame(yr=strftime(x[, 1], "%Y"), qu=quarters(x[, 1]), val=x[, 2])
  d <- stats::aggregate(val ~ yr + qu, d, FUN=mean, na.rm=TRUE)
  d$qu <- as.character(d$qu)
  d$qu[d$qu == "Q1"] <- "01-01"
  d$qu[d$qu == "Q2"] <- "04-01"
  d$qu[d$qu == "Q3"] <- "07-01"
  d$qu[d$qu == "Q4"] <- "10-01"
  d$date <- as.Date(with(d, paste(yr, qu, sep="-")))
  d <- d[order(d$date), c("date", "val")]
  colnames(d) <- names(x)
  rownames(d) <- NULL
  return(d)
}


# The .ReduceAmp function was modified from 'Signal Amplitude Reduction fx'.
#
# Copyright (c) 2011 Darin McCoy
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
.ReduceAmp <- function(signal, reduction) {
  if (length(signal) == 0 || !is.numeric(signal))
    stop("Problem with 'signal' argument")
  if (length(reduction) != 1 || !is.numeric(reduction))
    stop("Problem with 'reduction' argument")
  tmp.signal <- signal / reduction
  output.signal <- mean(signal) - mean(tmp.signal) + tmp.signal
  return(output.signal)
}
