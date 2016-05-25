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


# The .ReduceAmp function was modified from 'Signal Amplitude Reduction fx'
# authored by Darin McCoy [Copyright (c) 2011] and covered by the BSD License:
# http://www.mathworks.com/matlabcentral/fileexchange/view_license?file_info_id=32409
.ReduceAmp <- function(signal, reduction) {
  if (length(signal) == 0 || !is.numeric(signal))
    stop("Problem with 'signal' argument")
  if (length(reduction) != 1 || !is.numeric(reduction))
    stop("Problem with 'reduction' argument")
  tmp.signal <- signal / reduction
  output.signal <- mean(signal) - mean(tmp.signal) + tmp.signal
  return(output.signal)
}
