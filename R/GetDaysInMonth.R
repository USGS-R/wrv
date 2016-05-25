GetDaysInMonth <- function(x) {
  d <- as.Date(paste0(x, "28"), format="%Y%m%d")
  m <- format(d, format="%m")
  for (i in seq_along(d)) {
    while (format(d[i], format="%m") == m[i]) {
      d[i] <- d[i] + 1L
    }
  }
  return(as.integer(format(d - 1L, format="%d")))
}
