SummariseBudget <- function(budget,
                            desc=c("wells", "drains", "river leakage")) {

  if (!inherits(budget, "list")) {
    budget <- budget[1]
    if (is.character(budget) & file.access(budget) == 0)
      budget <- ReadModflowBinary(budget, "flow")
    else
      stop("problem with 'budget' argument")
  }
  desc <- match.arg(desc, several.ok=TRUE)

  budget.desc <- as.factor(vapply(budget, function(i) i$desc, ""))
  is.desc.not.included <- !desc %in% levels(budget.desc)
  if (any(is.desc.not.included ))
    warning(paste("missing flow variable(s) in budget file:",
                  paste(desc[is.desc.not.included], collapse=", ")))
  budget <- budget[budget.desc %in% desc]
  if (length(budget) == 0)
    stop("flow variable(s) can not be found in the budget file")

  descs <- vapply(budget, function(i) make.names(i$desc), "")

  .Summarise <- function(b, desc) {
    FUN <- function(j) {
      d <- data.frame(desc=j$desc, kper=j$kper, kstp=j$kstp, id=NA,
                      flow=j$d[, make.names(j$desc)], delt=j$delt,
                      pertim=j$pertim, totim=j$totim, stringsAsFactors=FALSE)
      if ("id" %in% colnames(j$d)) d$id <- as.integer(j$d[, "id"])
      return(d)
    }
    d <- bind_rows(lapply(desc,
                          function(i) bind_rows(lapply(b[desc == i], FUN))))
    d$desc <- as.factor(d$desc)
    d <- summarise(group_by(d, desc, kper, kstp, id),
                   delt=delt[1], pertim=pertim[1], totim=totim[1],
                   count=length(flow),
                   flow.sum=sum(flow),
                   flow.mean=mean(flow),
                   flow.median=median(flow),
                   flow.sd=sd(flow))
    return(d)
  }

  b <- budget
  for (i in seq_along(b)) {
    b[[i]]$d[b[[i]]$d[, descs[i]] < 0, descs[i]] <- 0
  }
  d <- mutate(.Summarise(b, desc), flow.dir="in")

  b <- budget
  for (i in seq_along(b)) {
    b[[i]]$d[b[[i]]$d[, descs[i]] > 0, descs[i]] <- 0
  }
  d <- bind_rows(d, mutate(.Summarise(b, desc), flow.dir="out"))

  d$flow.dir <- as.factor(d$flow.dir)

  return(d)
}
