RmSmallCellChunks <- function(r) {

  ext <- extent(r)
  new.ext <- extent(c(ext@xmin - res(r)[1], ext@xmax + res(r)[1],
                      ext@ymin - res(r)[2], ext@ymax + res(r)[2]))
  new.r <- extend(r, new.ext)

  r.values <- new.r[]
  r.clump <- clump(new.r, directions=4)
  chunk.numbers <- r.clump[]
  chunks <- unique(na.omit(chunk.numbers))
  FUN <- function(i) sum(chunk.numbers == i, na.rm=TRUE)
  chunk.sizes <- vapply(chunks, FUN, 0)

  biggest.chunk <- chunks[which(chunk.sizes == max(chunk.sizes))]
  n <- length(biggest.chunk)
  if (n > 1L)
    warning(paste("There are", n, "raster chunks with largest area."))

  chunk.numbers[!is.na(chunk.numbers) & !chunk.numbers %in% biggest.chunk] <- NA
  r.values[is.na(chunk.numbers)] <- NA
  new.r[] <- r.values

  r <- crop(new.r, ext)

  return(r)
}
