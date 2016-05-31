GetSeasonalMult <- function(x, reduction, d.in.mv.ave, tr.stress.periods) {

  if (!is.data.frame(x) || !inherits(x[, 1], "Date") || !is.numeric(x[, 2]))
    stop("Problem with 'x' argument")
  if (!is.numeric(reduction) || reduction < 0)
    stop("Problem with 'reduction' argument")
  if (!is.numeric(d.in.mv.ave) || d.in.mv.ave <= 0)
    stop("Problem with 'd.in.mv.ave' argument")
  if (!inherits(tr.stress.periods, "Date"))
    stop("Problem with 'tr.stress.periods' argument")

  f <- approxfun(x[, 1], x[, 2])
  d <- data.frame(YearMonth=head(tr.stress.periods, -1))
  FUN <- function(i) {
    vol <- integrate(f, i - d.in.mv.ave, i, subdivisions=1000L, rel.tol=0.001)
    return(vol$value / d.in.mv.ave)
  }
  d[, 2] <- vapply(d[, 1], FUN, 0)  # moving average
  d <- .GetQtrlyMean(d)  # seasonal mean
  d[, 2] <- .ReduceAmp(d[, 2], reduction)  # amplitude reduction
  d$multiplier <- d[, 2] / mean(d[, 2])  # seasonal multiplier
  d <- d[, c(1, 3)]
  colnames(d) <- c(colnames(x)[1], "multiplier")
  rownames(d) <- NULL

  return(d)
}


.GetQtrlyMean <- function(x) {
  if (!is.data.frame(x) || !inherits(x[, 1], "Date") || !is.numeric(x[, 2]))
    stop("Problem with 'x' argument")
  d <- data.frame(yr=strftime(x[, 1], "%Y"), qu=quarters(x[, 1]), val=x[, 2])
  d <- aggregate(val ~ yr + qu, d, FUN=mean, na.rm=TRUE)
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
