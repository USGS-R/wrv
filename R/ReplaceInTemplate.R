ReplaceInTemplate <- function(text, replacement) {

# This function was derived from the sensitivity::template.replace function,
# accessed on Feb 6, 2015 (http://cran.r-project.org/web/packages/sensitivity/)

  if (missing(replacement)) replacement <- list()

  for (i in seq_along(text)) {

    for (j in names(replacement)) {
      pattern <- sub("KEY", j, "\\$\\(KEY\\)", perl=TRUE)
      text[i] <- gsub(pattern, paste(replacement[[j]]), text[i], perl=TRUE)
    }

    repeat {
      reg <- regexpr("@\\{.+?\\}", text[i], perl=TRUE)
      if (reg == -1) break

      match.first <- as.integer(reg)
      match.last <- match.first + attr(reg, "match.length") - 1L
      match.text <- substr(text[i], match.first + 2L, match.last - 1L)

      val.match.text <- eval(parse(text=match.text))

      line.begin <- substr(text[i], 1, match.first - 1L)
      line.middle <- paste(val.match.text)
      line.end <- substr(text[i], match.last + 1L, nchar(text[i]))
      text[i] <- paste0(line.begin, line.middle, line.end)
    }

  }

  return(text)
}
