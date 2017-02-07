## ----setup, include=FALSE------------------------------------------------
t0 <- Sys.time()
try(knitr::opts_chunk$set(tidy=FALSE, comment="#", fig.align="center"), silent=TRUE)
grDevices::pdf.options(useDingbats=FALSE)
library(wrv)
library(inlmisc)
library(raster)
options(preferRaster=TRUE, scipen=0, digits=2)

# Device dimension in inches (width, height)
fin.graph         <- c(7.17, 7.17)
fin.graph.short   <- c(7.17, 3.50)
fig.graph.small   <- c(3.50, 3.50)
fin.map           <- c(7.17, 9.31)
fin.map.0         <- c(7.17, 8.77)
fin.map.s         <- c(7.17, 5.22)
fin.map.s.0       <- c(7.17, 4.68)
fin.map.n         <- c(7.17, 6.97)
fin.map.n.small   <- c(3.50, 3.60)
fin.map.n.small.0 <- c(3.50, 3.30)
fin.cs            <- c(7.17, 5.26)
fin.cs.0          <- c(7.17, 4.68)

# Extreme coordinates of plotting region (x1, x2, y1, y2)
usr.map     <- c(2451504, 2497815, 1342484, 1402354)
usr.map.s   <- c(2472304, 2497015, 1343284, 1358838)
usr.map.n.1 <- c(2463000, 2475356, 1386500, 1398856)
usr.map.n.2 <- c(2467500, 2479856, 1376500, 1388856)
usr.map.n.3 <- c(2466696, 2479052, 1366501, 1378857)
usr.map.n.4 <- c(2471500, 2483856, 1356482, 1368838)

# Unit conversions
m.to.km <- 0.001
m.to.mi <- 0.000621371

# Map credit
credit <- paste("Base derived from U.S. Geological Survey National Elevation Dataset 10-meter digital elevation model.",
                "Idaho Transverse Mercator projection; North American Datum of 1983.", sep="\n")

## ----table_canal_seep, echo=FALSE, results="asis"------------------------
d <- data.frame(canals@data, length=SpatialLinesLengths(canals))
d <- aggregate(d$length, by=list(d$EntityName), sum)
names(d) <- c("EntityName", "length")
d$id <- seq_len(nrow(d))
d$length_km <- d$length * m.to.km
d$length_mi <- d$length * m.to.mi
d$length <- NULL
d <- merge(d, canal.seep)
d$SeepFrac <- d$SeepFrac * 100
columns <- c("Name",
             "Canal \\\\ No.",
             "Total length \\\\ (km)",
             "Total length \\\\ (mi)",
             "Percent \\\\ seepage loss")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Summary description of canal system attributes."
cap2 <- c("\\textbf{Canal No.}: is an identifier used to locate the canal system on the map in \\hyperref[fig:map_canals]{figure~\\ref{fig:map_canals}}.",
          "\\textbf{Total length}: is the length of the canal system.",
          "\\textbf{Percent seepage loss}: is the percentage of water diverted to the canal headgate that infiltrates the canal bed and banks and recharges the aquifer system.",
          "\\textbf{Abbreviations}: km, kilometer; mi, mile")
tbl <- xtable::xtable(d, label="table_canal_seep")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- c(0, 0, 0, 1, 1, 0)
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      size="\\small")

## ----include=FALSE-------------------------------------------------------
v <- "Canals in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_canals_full, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, bg.image.alpha=0.6, dms.tick=TRUE,
        rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit,
        scale.loc="bottomleft")
cols <- c("#33333366", "#FBB829", "#000000")
plot(alluvium.extent, border="#FFFFFFCC", col=cols[1], add=TRUE)
lines(canals, col=cols[2])
plot(extent(usr.map.n.1), add=TRUE)
plot(extent(usr.map.n.2), add=TRUE)
plot(extent(usr.map.n.3), add=TRUE)
plot(extent(usr.map.n.4), add=TRUE)
plot(extent(usr.map.s),   add=TRUE)
x <- c(usr.map.n.1[2], usr.map.n.2[2], usr.map.n.3[2], usr.map.n.4[2], usr.map.s[2])
y <- c(usr.map.n.1[4], usr.map.n.2[4], usr.map.n.3[4], usr.map.n.4[4], usr.map.s[4])
text(x, y, LETTERS[seq_along(x)], adj=c(1.4, 1.4), cex=0.6, lwd=0.75, font=4)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
leg <- c("Aquifer extent", "Canal", "Inset areas, fig. G2.")
legend("topleft", leg, pch=c(22, NA, 22), lwd=c(NA, 1, NA), col=cols,
       pt.bg=c(cols[1], NA, NA), pt.cex=1.5, inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, bg="#FFFFFFE7")
AddInsetMap(idaho, width=1, main.label=list("IDAHO", adj=c(-0.4, -4.9)),
            sub.label=list("Map area", adj=c(0.5, 2.5)), loc="topright")

## ----echo=FALSE----------------------------------------------------------
Pal <- function(...) {
  cols1 <- rainbow(..., s=0.5, v=1)
  cols2 <- rainbow(..., s=1, v=0.5)
  is.even <- seq_along(cols1) %% 2 == 0
  cols1[is.even] <- cols2[is.even]
  return(cols1)
}

## ----echo=FALSE----------------------------------------------------------
cols <- Pal(max(levels(r.canals)[[1]]$ID))
FUN <- function(usr, credit=NULL, pos=1, max.dev.dim=c(43, 56)) {
  r <- ratify(crop(deratify(r.canals, att="ID"), extent(usr), snap="out"))
  PlotMap(r, att="ID", xlim=usr[1:2], ylim=usr[3:4], bg.image=hill.shading,
          bg.image.alpha=0.6, dms.tick=TRUE, max.dev.dim=max.dev.dim,
          credit=credit, rivers=list(x=streams.rivers), lakes=list(x=lakes),
          col=cols[as.integer(levels(r)[[1]][, 1])], draw.key=FALSE,
          scale.loc="bottomleft")
  if (nrow(levels(r)[[1]]) > 0) {
    poly <- rasterToPolygons(r, dissolve=TRUE)
    xy <- suppressWarnings(getSpatialPolygonsLabelPoints(poly))
    text(xy, labels=poly@data[, 1], col="#333333", cex=0.6, pos=4, offset=0.4)
  }
  plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
  text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=pos, offset=0.4)
  if (length(ids <- as.integer(levels(r)[[1]]$ID)) > 0)
    legend("topright", as.character(ids), fill=cols[ids], border=NA, inset=0.02,
           cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7",
           title=expression(bold("Canal")))
}

## ----map_canals_a, echo=FALSE, results="asis", fig.width=fin.map.n.small.0[1], fig.height=fin.map.n.small.0[2]----
FUN(usr.map.n.1, pos=3, max.dev.dim=c(21, 56))

## ----map_canals_b, echo=FALSE, results="asis", fig.width=fin.map.n.small.0[1], fig.height=fin.map.n.small.0[2]----
FUN(usr.map.n.2, pos=3, max.dev.dim=c(21, 56))

## ----map_canals_c, echo=FALSE, results="asis", fig.width=fin.map.n.small.0[1], fig.height=fin.map.n.small.0[2]----
FUN(usr.map.n.3, max.dev.dim=c(21, 56))

## ----map_canals_d, echo=FALSE, results="asis", fig.width=fin.map.n.small.0[1], fig.height=fin.map.n.small.0[2]----
FUN(usr.map.n.4, pos=3, max.dev.dim=c(21, 56))

## ----include=FALSE-------------------------------------------------------
v <- "Canal systems mapped to the model grid (\\textit{\\textbf{A}}) north of Ketchum, (\\textit{\\textbf{B}}) south of Ketchum and north of Gimlet, (\\textit{\\textbf{C}}) south of Gimlet and north of Hailey, (\\textit{\\textbf{D}}) south of Hailey and north of Bellevue, and (\\textit{\\textbf{E}}) south of Bellevue. \\label{fig:map_canals}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_canals_e, echo=FALSE, results="asis", fig.width=fin.map.s.0[1], fig.height=fin.map.s.0[2]----
FUN(usr.map.s, credit)

## ----include=FALSE-------------------------------------------------------
v <- "The Bypass Canal and City of Bellevue wastewater treatment plant ponds mapped to the model grid."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_bypass_wwtp, echo=FALSE, fig.width=fin.map.s.0[1], fig.height=fin.map.s.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
r <- raster(alluvium.thickness)
r[!is.na(rasterize(bypass.canal, r)[])] <- 1L
r[rasterize(bellevue.wwtp.ponds, r, getCover = TRUE) > 0] <- 2L
r <- ratify(r)
d <- data.frame(RechSite = c("Bypass Canal", "Bellevue WWTP Ponds"))
levels(r) <- cbind(levels(r)[[1]], d)
cols <- c("#FAB236", "#F02311")
PlotMap(r, att="RechSite", xlim=usr.map.s[1:2], ylim=usr.map.s[3:4],
        bg.image=hill.shading, bg.image.alpha=0.6, dms.tick=TRUE, col=cols,
        roads=list(x=major.roads), rivers=list(x=streams.rivers),
        lakes=list(x=lakes), draw.key=FALSE, credit=credit, scale.loc="bottomleft")
plot(alluvium.extent, border="#FFFFFF7F", add=TRUE)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
lab <- cbind(map.labels@coords, map.labels@data)
for (i in seq_len(nrow(lab))) {
  text(lab$x[i], lab$y[i], labels=lab$label[i], cex=lab$cex[i], col=lab$col[i],
       font=lab$font[i], srt=lab$srt[i])
}
text(getSpatialLinesMidPoints(rgeos::gLineMerge(bypass.canal)), labels="Bypass Canal",
     cex=0.5, col="#333333", srt=80, font=1, pos=4)
text(suppressWarnings(getSpatialPolygonsLabelPoints(bellevue.wwtp.ponds)),
     labels="Bellevue WWTP Ponds", cex=0.5, col="#333333", font=1, pos=2)
legend("topright", as.character(levels(r)[[1]]$RechSite), fill=cols,
       border=NA, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")

## ----table_irr_lands_year, echo=FALSE, results="asis"--------------------
d <- irr.lands.year
d[d[, 1] == d[, 2], 2] <- NA
d <- data.frame(d[1:8, ], d[9:16, ])
columns <- rep(c("Year", "Proxy year"), 2)
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Applied land use classification maps of irrigated lands during the model simulation period (1995--2010)."
cap2 <- "\\textbf{Abbreviations}: NA, not applicable"
tbl <- xtable::xtable(d, label="table_irr_lands_year")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- "lcc|cc"
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      size="\\small", NA.string="NA")

## ----echo=FALSE----------------------------------------------------------
FUN <- function(i) {
  PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
          bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes),
          credit=credit, scale.loc="bottomleft")
  plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
  cols <- c("#90AB76", "#AF8DC3")
  is.irr <- irr.lands[[i]]@data$Status == "irrigated"
  plot(irr.lands[[i]][ is.irr, ], col=cols[1], border=NA, add=TRUE)
  plot(irr.lands[[i]][!is.irr, ], col=cols[2], border=NA, add=TRUE)
  plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
  text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
  legend("topright", c("Irrigated", "Semi-irrigated"), fill=cols,
         border=NA, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")
  box(lwd=0.5)
}

## ----map_land_use_a, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("1996")

## ----include=FALSE-------------------------------------------------------
v <- "Land use classification maps of irrigated lands during the (\\textit{\\textbf{A}}) 1996, (\\textit{\\textbf{B}}) 2000, (\\textit{\\textbf{C}}) 2002, (\\textit{\\textbf{D}}) 2006, and (\\textit{\\textbf{E}}) 2008, (\\textit{\\textbf{F}}) 2009, and (\\textit{\\textbf{G}}) 2010 growing seasons, Wood River Valley and surrounding areas, Idaho. \\label{fig:map_land_use}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_land_use_b, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("2000")

## ----map_land_use_c, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("2002")

## ----map_land_use_d, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("2006")

## ----map_land_use_e, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("2008")

## ----map_land_use_f, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("2009")

## ----map_land_use_g, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN("2010")

## ----include=FALSE-------------------------------------------------------
v <- "Wetlands and non-irrigated public land parcels in the Wood River Valley and surrounding areas, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_wetlands_parcels, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
        bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes),
        credit=credit, scale.loc="bottomleft")
plot(public.parcels, col="#CDBB99C0", border="#755C3BC0", add=TRUE)
plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
plot(wetlands, col="#59A80F", border=NA, add=TRUE)
legend("topright", c("Wetlands", "Public parcels"), fill=c("#59A80F", "#CDBB99C0"),
       border=c(NA, "#755C3BC0"), inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5,
       xpd=NA, bg="#FFFFFFE7")
box(lwd=0.5)

## ----echo=FALSE----------------------------------------------------------
x <- irr.entities@data
entity.names <- levels(x$EntityName)
d <- data.frame(entity.name=entity.names, id=seq_along(entity.names))
FUN <- function(i) paste(sort(unique(x$Source[x$EntityName == i])), collapse=", ")
d$source <- sapply(entity.names, FUN)

PrintTable <- function(idxs, is.last.hline, label) {
  d <- d[idxs, ]
  columns <- c("Name", "Entity No.", "Water source")
  colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
  cap1 <- "Irrigation entities and their respective water sources."
  cap2 <- c("\\textbf{Entity No.}: is an identifier used to locate the irrigation entity on the map in \\hyperref[fig:map_entities]{figure~\\ref{fig:map_entities}}.",
            "\\textbf{Water source}: is the water source classification for irrigated and semi-irrigated lands where:",
            "`GW Only' indicates irrigation water exclusively from groundwater sources;",
            "`SW Only' indicates irrigation water exclusively from surface-water sources; and",
            "`Mixed' indicates a combination of groundwater and surface-water sources.")
  tbl <- xtable::xtable(d, label=label)
  if (idxs[1] == 1L)
    xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
  else
    xtable::caption(tbl) <- sprintf("%s---Continued", cap1)
  xtable::align(tbl) <- "rlcl"
  hline.after <- if (is.last.hline) c(-1, 0, nrow(tbl)) else c(-1, 0)
  print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
        sanitize.colnames.function=function(x){x}, size="\\small",
        hline.after=hline.after)
}

## ----table_irr_entities, echo=FALSE, results="asis"----------------------
PrintTable(1:(n <- 50L), FALSE, label <- "table_irr_entities")

## ----echo=FALSE, results="asis"------------------------------------------
cat("\\addtocounter{table}{-1}\n")
PrintTable((n + 1L):nrow(d), TRUE, paste0(label, "_1"))

## ----echo=FALSE----------------------------------------------------------
polys <- irr.entities
polys@data$id <- as.integer(polys@data$EntityName)
cols <- Pal(max(polys@data$id), alpha=0.8)
FUN <- function(usr) {
  p <- SetPolygons(polys, extent(usr), "gIntersection")
  PlotMap(crs(hill.shading), xlim=usr[1:2], ylim=usr[3:4], bg.image=hill.shading,
          bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers),
          lakes=list(x=lakes), credit=credit, scale.loc="bottomleft")
  plot(p, col=cols[p@data$id], border=NA, add=TRUE)
  plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
  text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
  plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
  xy <- suppressWarnings(getSpatialPolygonsLabelPoints(p))
  text(xy, labels=p@data$id, col="#333333", cex=0.6)
  ids <- sort(unique(p@data$id))
  legend("topright", as.character(ids), fill=cols[ids], border=NA, inset=0.02,
         cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7", ncol=2,
         title=expression(bold("Irrigation Entity")))
}

## ----map_entities_a, echo=FALSE, results="asis", fig.width=fin.map.n[1], fig.height=fin.map.n[2]----
FUN(usr.map.n.1)

## ----include=FALSE-------------------------------------------------------
v <- "Irrigation entities (\\textit{\\textbf{A}}) north of Ketchum, (\\textit{\\textbf{B}}) south of Ketchum and north of Gimlet, (\\textit{\\textbf{C}}) south of Gimlet and north of Hailey, (\\textit{\\textbf{D}}) south of Hailey and north of Bellevue, and (\\textit{\\textbf{E}}) south of Bellevue. \\label{fig:map_entities}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_entities_b, echo=FALSE, results="asis", fig.width=fin.map.n[1], fig.height=fin.map.n[2]----
FUN(usr.map.n.2)

## ----map_entities_c, echo=FALSE, results="asis", fig.width=fin.map.n[1], fig.height=fin.map.n[2]----
FUN(usr.map.n.3)

## ----map_entities_d, echo=FALSE, results="asis", fig.width=fin.map.n[1], fig.height=fin.map.n[2]----
FUN(usr.map.n.4)

## ----map_entities_e, echo=FALSE, results="asis", fig.width=fin.map.s.0[1], fig.height=fin.map.s.0[2]----
FUN(usr.map.s)

## ----include=FALSE-------------------------------------------------------
v <- "Irrigation entities mapped to irrigated lands in the southern part of the model grid during the 2008 growing season (April--October)."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_entities_2008, echo=FALSE, fig.width=fin.map.s.0[1], fig.height=fin.map.s.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
r <- deratify(rs.entities[["200804"]], "ID")
at <- seq_len(max(r[], na.rm=TRUE))
cols <- Pal(max(r[], na.rm=TRUE), alpha=0.8)
PlotMap(r, breaks=seq(min(at) - 0.5, max(at) + 0.5), xlim=usr.map.s[1:2],
        ylim=usr.map.s[3:4], bg.image=hill.shading, bg.image.alpha=0.6,
        dms.tick=TRUE, col=cols, labels=list(at=at), draw.key=FALSE,
        rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit,
        scale.loc="bottomleft")
poly <- rasterToPolygons(crop(r, extent(usr.map.s)), dissolve=TRUE)
plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
xy <- suppressWarnings(getSpatialPolygonsLabelPoints(poly))
text(xy, labels=poly@data[, 1], col="#333333", cex=0.6, pos=4, offset=0.4)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
ids <- unique(crop(r, extent(usr.map.s)))
legend("topright", as.character(ids), fill=cols[ids], ncol=2,
       border=NA, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA,
       bg="#FFFFFFE7", title=expression(bold("Irrigation Entity")))

## ----include=FALSE-------------------------------------------------------
v <- "Irrigation entity water sources in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_sources, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
        bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes),
        credit=credit, scale.loc="bottomleft")
polys <- irr.entities
cols <- c("#66C2A5CC", "#FC8D62CC", "#8DA0CBCC")
plot(polys, col=cols[as.integer(polys@data$Source)], border=NA, add=TRUE)
plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
legend("topright", levels(polys@data$Source), fill=cols,
       border=NA, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7",
       title=expression(bold("Source")))

## ----include=FALSE-------------------------------------------------------
v <- "Water supply and fate of water in municipal areas."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----include=FALSE-------------------------------------------------------
v <- "Water supply and fate of water within subdivisions with community water systems."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----include=FALSE-------------------------------------------------------
v <- "Water supply and fate of water within residential areas served by individual wells."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----include=FALSE-------------------------------------------------------
v <- "Groundwater points of diversion in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_pod, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
        dms.tick=TRUE, bg.image.alpha=0.6, rivers=list(x=streams.rivers), lakes=list(x=lakes),
        credit=credit, scale.loc="bottomleft")
plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
is.obs <- pod.wells@data$WMISNumber %in% unique(div.gw$WMISNumber)
cols <- c("#9061C2", "#FBB829")
pchs <- c(24, 21)
pt.cexs <- c(0.5, 0.8)
points(pod.wells[!is.obs, ], pch=pchs[1], cex=pt.cexs[1], lwd=0.5, col=NA, bg=cols[1])
points(pod.wells[ is.obs, ], pch=pchs[2], cex=pt.cexs[2], lwd=0.5, col=NA, bg=cols[2])
leg <- paste(c("Unmeasured", "Measured"), "groundwater diversion")
legend("topright", leg, col=NA, pt.bg=cols, pch=pchs, pt.cex=pt.cexs, pt.lwd=0.5,
       inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")

