ToScientific <- function(x, digits=format.info(as.numeric(x))[2],
                         lab.type=c("latex", "plotmath")) {

  lab.type <- match.arg(lab.type)
  x[is.zero <- x == 0] <- NA
  idxs <- which(is.finite(x))

  m <- rep(NA, length(x))
  n <- m
  n[idxs] <- floor(log(abs(x[idxs]), 10))
  m[idxs] <- sprintf("%0.*f", digits, x[idxs] / 10^n[idxs])
  if (lab.type == "latex") {
    s <- rep(NA, length(x))
    s[idxs] <- sprintf("$%s \\times 10^{%d}$", m[idxs], n[idxs])
    s[is.zero] <- "0"
  } else {
    FUN <- function(i) {
      if (i %in% which(is.zero)) return(quote(0))
      if (is.na(x[i])) return("")
      return(substitute(paste(M, " x ", 10^N), list(M=m[i], N=n[i])))
    }
    s <- lapply(seq_along(x), FUN)
    s <- do.call("expression", s)
  }
  return(s)
}
