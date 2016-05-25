DownloadFile <- function(url, dest.dir=tempdir(), mode=NULL, extract=TRUE,
                         max.attempts=10L, wait.time=30) {

  if (!requireNamespace("RCurl", quietly=TRUE))
    stop("package 'RCurl' is not available")

  cainfo <- system.file("CurlSSL", "cacert.pem", package="RCurl")
  curl.options <- list(cainfo=cainfo, followlocation=TRUE, header=TRUE,
                       nobody=TRUE, ftp.use.epsv=TRUE, useragent="R",
                       timeout=100, ftp.response.timeout=100)

  i <- 1L
  repeat {
    handle <- RCurl::getCurlHandle()
    invisible(try(RCurl::getURL(url, .opts=curl.options, curl=handle),
                  silent=TRUE))
    info <- RCurl::getCurlInfo(handle)
    if (info$response.code %in% c(200L, 350L)) {
      break
    } else {
      if (i > max.attempts)
        stop("exceeded the maximum number of attempts to download a file")
      i <- i + 1L
      Sys.sleep(wait.time)
    }
  }

  eff.url <- info$effective.url
  pos <- regexpr("\\.([[:alnum:]]+)$", eff.url)
  ext <- ifelse(pos > -1L, paste0(".", substring(eff.url, pos + 1L)), "")

  # Mode is only an issue for Windows OS
  if (is.null(mode)) {
    if (is.null(info$content.type)) {  # assume mode when missing conent type
      txt.ext <- paste0(".", c("txt", "csv", "tsv", "htm", "html", "xml",
                               "asc", "lst", "log", "json"))
      mode <- if (ext %in% txt.ext) "w" else "wb"  # TODO(jcf): find better way
    } else {
      mode <- if (grepl("^text/", info$content.type)) "w" else "wb"
    }
  }

  temp.file <- file.path(dest.dir, basename(eff.url))

  curl.options <- list(cainfo=cainfo, followlocation=TRUE)

  i <- 1L
  repeat {
    con <- RCurl::CFILE(temp.file, mode=mode)
    status <- try(RCurl::curlPerform(url=eff.url, writedata=con@ref,
                                     .opts=curl.options), silent=TRUE)
    RCurl::close(con)
    if (status == 0L) {
      break
    } else {
      if (i > max.attempts)
        stop(paste("exceeded max number of attempts:", status))
      i <- i + 1L
      Sys.sleep(wait.time)
    }
  }

  if (extract && ext %in% c(".zip", ".gz", ".bz2", ".xz")) {
    if (ext == ".zip") {
      extracted.files <- unzip(temp.file, exdir=dest.dir)
    } else {
      idx <- which(c(".gz", ".bz2", ".xz") == ext)
      compressed <- c("gzip", "bzip2", "xz")[idx]
      untar(temp.file, exdir=dest.dir, compressed=compressed)
      extracted.files <- untar(temp.file, list=TRUE, compressed=compressed)
    }
    unlink(temp.file)
    temp.file <- extracted.files
  }

  invisible(temp.file)
}


.GetCommonDir <- function(paths) {
  if (length(paths) < 2)
    return()
  paths <- normalizePath(paths, "/")
  chunks <- strsplit(paths, "/")
  i <- 1L
  repeat {
    chunk <- sapply(chunks, function(x) x[i])
    if (!all(chunk %in% chunk[1]))
      break
    i <- i + 1L
  }
  path <- paste(chunks[[1]][seq_len(i - 1L)], collapse="/")
  return(path)
}
