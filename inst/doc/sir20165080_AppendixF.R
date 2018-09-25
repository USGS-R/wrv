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
m.to.ft <- 3.28084
m.to.km <- 0.001
m.to.mi <- 0.000621371

# Map credit
credit <- paste("Base derived from U.S. Geological Survey National Elevation Dataset 10-meter digital elevation model.",
                "Idaho Transverse Mercator projection; North American Datum of 1983.", sep="\n")

## ----echo=FALSE----------------------------------------------------------
FUN <- function(is.irr) {
  PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
          bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes),
          credit=credit, scale.loc="bottomleft")
  plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
  cols <- c("#d95f02", "#1b9e77")
  labs <- c("Non-irrigated", "Irrigated and semi-irrigated")
  if (is.irr) {
    p <- as(irr.lands[["2008"]], "SpatialPolygons")
    p1 <- SetPolygons(p, alluvium.extent, "gIntersection", 0.001)
    p0 <- SetPolygons(alluvium.extent, p1, "gDifference", 0.001)
    plot(p0, col=cols[1], border=NA, add=TRUE)
    plot(p1, col=cols[2], border=NA, add=TRUE)
  } else {
    plot(alluvium.extent, col=cols[1], border=NA, add=TRUE)
    cols <- cols[1]
    labs <- labs[1]
  }
  plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
  text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
  legend("topleft", labs, fill=cols, border=NA, inset=0.02, cex=0.7, box.lty=1,
         box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")
  if (!is.irr)
    AddInsetMap(idaho, width=1, main.label=list("IDAHO", adj=c(-0.4, -4.9)),
                sub.label=list("Map area", adj=c(0.5, 2.5)), loc="topright")
}

## ----map_land_use_a, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN(FALSE)

## ----include=FALSE-------------------------------------------------------
v <- "Irrigated, semi-irrigated, and non-irrigated lands in the Wood River Valley, Idaho, during (\\textit{\\textbf{A}}) November through March, and (\\textit{\\textbf{B}}) April through October, 2008. \\label{fig:map_land_use}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_land_use_b, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN(TRUE)

## ----include=FALSE-------------------------------------------------------
v <- "Spatial distribution of precipitation zones and weather stations in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_precip_zones, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
        dms.tick=TRUE, bg.image.alpha=0.6, rivers=list(x=streams.rivers),
        lakes=list(x=lakes), credit=credit, scale.loc="bottomleft")
plot(alluvium.extent, border="#FFFFFFCC", add=TRUE)
cols <- c("#66C2A5", "#FC8D62", "#8DA0CB")
polys <- SetPolygons(precip.zones, alluvium.extent, "gIntersection", 0.001)
plot(polys, col=cols, border=NA, add=TRUE)
plot(weather.stations, pch=23, cex=0.8, col="#333333", add=TRUE)
labs <- sprintf("%s\n(%s)", weather.stations@data$name, weather.stations@data$type)
text(weather.stations, labels=labs, col="#333333", cex=0.5, pos=1, offset=0.4)
leg <- paste(as.character(polys@data$PrecipZone), "precipitation zone")
legend("topright", leg, fill=cols, border=NA, inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")
leg <- "Weather stations"
legend("topleft", leg, pch=23, pt.bg="#333333", pt.cex=0.8, inset=0.02,
       cex=0.7, box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")

## ----include=FALSE-------------------------------------------------------
v <- "Daily average snow water equivalent recorded at weather stations in the Wood River Valley and surrounding areas, Idaho."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_swe, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- swe
mo <- as.integer(substr(d$MonthDay, 1, 2))
d$MonthDay[mo %in% 10:12] <- paste0("2003", d$MonthDay[mo %in% 10:12])
d$MonthDay[mo %in% 1:9] <- paste0("2004", d$MonthDay[mo %in% 1:9])
d[, 1] <- as.Date(d$MonthDay, "%Y%m%d")
d <- d[order(d$MonthDay), ]
col <- c("#1B9E77", "#D95F02", "#7570B3")
ylab <- paste("Snow water equivalent, in", c("meters", "inches"))
xlim <- range(d[, 1])
PlotGraph(d, xlim=xlim, ylab=ylab, col=col, fill="tozeroy", lty=1,
          seq.date.by="month", conversion.factor=39.3701,
          center.date.labels=TRUE)
xat <- seq(xlim[1], xlim[2], by="month")
at <- c(xat + diff(c(xat, xlim[2])) / 2, xlim[2])
xlabs <- sprintf("(%d)", c(10:12, 1:9))
axis.Date(1, at=at, tcl=0, cex.axis=0.6, lwd=-1, labels=xlabs, padj=1.5)
leg <- c("Chocolate Gulch SNOTEL", "Hailey HADS", "Picabo AgriMet")
legend("topright", leg, col=col, lty=1, inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, bg="#FFFFFFE7")

## ----echo=FALSE----------------------------------------------------------
elev <- weather.stations@data[, "elevation"]
names(elev) <- weather.stations@data[, "id"]
FUN <- function(i) {
  sprintf("%s~m (%s~ft)",
          formatC(elev[i], format="d", big.mark=","),
          formatC(elev[i] * m.to.ft, format="d", big.mark=","))
}

## ----echo=FALSE----------------------------------------------------------
FUN <- function(i, col) {
  d <- precipitation[precipitation$PrecipZone == i, ]
  d$PrecipZone <- NULL
  d[, 1] <- as.Date(paste0(d[, 1], "01"), "%Y%m%d")
  d <- rbind(d, d[nrow(d), , drop=FALSE])
  d[nrow(d), 1] <- d[nrow(d), 1] + diff(d[, 1])[1]
  ylab <- paste("Monthly precipitation, in", c("meters", "feet"))
  col <- c(col, "#333333")
  PlotGraph(d, ylab=ylab, col=col, fill="tozeroy", lty=1,
            conversion.factor=m.to.ft, center.date.labels=TRUE,
            seq.date.by="year")
  leg <- c("Adjusted for spring melt", "Recorded at weather station")
  legend("topright", leg, col=col, lty=1, inset=0.02, cex=0.7, box.lty=1,
         box.lwd=0.5, bg="#FFFFFFE7")
}

## ----graph_precip_a, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN("Ketchum", "#1B9E77")

## ----include=FALSE-------------------------------------------------------
v <- "Monthly precipitation depth at the (\\textit{\\textbf{A}}) Ketchum, (\\textit{\\textbf{B}}) Hailey, (\\textit{\\textbf{C}}) Picabo weather stations, Idaho. \\label{fig:graph_precip}"
v <- c(paste("Diagrams showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_precip_b, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN("Hailey", "#D95F02")

## ----graph_precip_c, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN("Picabo", "#7570B3")

## ----include=FALSE-------------------------------------------------------
v <- "Methods used to estimate evapotranspiration."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_et_method, echo=FALSE, fig.width=fin.graph, fig.height=1, fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- et.method
xlim <- as.Date(c("1995-01-01", "2011-01-01"))
x <- seq(xlim[1], xlim[2], "months")
y <- d$ETMethod[match(x, as.Date(paste0(d$YearMonth, "01"), format="%Y%m%d"))]
cols <- c("#FFFFFF", "#7380B5", "#83B6AC", "#C9C26C", "#F3A2AC")
par(mar=c(2.3, 1.1, 1.5, 1.1), mgp=c(3.2, 0.2, 0))
plot(NA, xlim=xlim, ylim=c(0, 1), xaxs="i", yaxs="i", xaxt="n", yaxt="n",
     xlab=NA, ylab=NA, cex.lab=0.8, cex.axis=0.8, las=2, frame.plot=FALSE)
rect(head(x, -1), 0,  x[-1], 1, col=cols[y], border=NA)
tcl <- 7.2 / par("cra")[2]
at <- seq(xlim[1], xlim[2], "years")
axis.Date(1, at=at, cex.axis=0.7, tcl=tcl, lwd=-1, lwd.ticks=0.5, labels=FALSE)
axis.Date(1, at=at + diff(c(at, xlim[2])) / 2, tcl=0, cex.axis=0.7, lwd=-1)
axis.Date(3, at=at, cex.axis=0.7, tcl=tcl, lwd=-1, lwd.ticks=0.5, labels=FALSE)
idxs <- c(1, 3, 5, 2, 4)
legend(mean(par("usr")[1:2]), 1, levels(y)[idxs], fill=cols[idxs], horiz=TRUE,
       cex=0.7, bty="n", xpd=TRUE, xjust=0.5, yjust=0,
       border=c("#333333", cols[idxs][-1]))
box(lwd=0.5)

## ----table_et_picabo-ar, echo=FALSE, results="asis"----------------------
covers <- c("100 percent impervious", "Grass turf (lawns) - irrigated", "Alfalfa - less frequent cuttings",
            "Bare soil", "Cottonwoods", "Grass Pasture - high management", "Mulched soil (including grain stubble)",
            "Open water - shallow systems", "Range grass - early short season", "Sage brush", "Spring grain - irrigated",
            "Sweet corn - late plant", "Wetlands - narrow stands", "Willows", "Winter grain", "Range (not including impervious)")
d <- data.frame(covers,
                nov=c(0.00, 0.21, 0.47, 0.40, 0.32, 0.21, 0.30, 0.68, 0.30, 0.30, 0.30, 0.42, 0.32, 0.33, 0.44, 0.47),
                dec=c(0.00, 0.11, 0.29, 0.28, 0.20, 0.11, 0.20, 0.29, 0.20, 0.20, 0.20, 0.29, 0.20, 0.20, 0.28, 0.18),
                jan=c(0.00, 0.14, 0.36, 0.36, 0.25, 0.14, 0.25, 0.34, 0.25, 0.25, 0.25, 0.36, 0.25, 0.25, 0.35, 0.22),
                feb=c(0.00, 0.23, 0.58, 0.57, 0.39, 0.23, 0.39, 0.63, 0.39, 0.39, 0.39, 0.58, 0.39, 0.39, 0.57, 0.40),
                mar=c(0.00, 0.45, 0.88, 0.81, 0.71, 0.45, 0.67, 1.32, 0.70, 0.70, 0.69, 0.88, 0.71, 0.72, 1.07, 0.87))
columns <- c("Land cover", "Nov \\\\ (mm/d)", "Dec \\\\ (mm/d)", "Jan \\\\ (mm/d)", "Feb \\\\ (mm/d)", "Mar \\\\ (mm/d)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Average monthly land cover evapotranspiration rates at the Picabo AgriMet weather station as calculated by the Allen and Robison method."
cap2 <- "\\textbf{Abbreviations}: mm/d, millimeters per day"
tbl <- xtable::xtable(d, label="table_et_picabo-ar")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 2
xtable::align(tbl) <- c("l", "l", "c", "c", "c", "c", "c")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      size="\\small")

## ----table_land-cover-ar, echo=FALSE, results="asis"---------------------
d <- data.frame(yr=1995:2010,
                covers=c(rep(2001, 6), NA, 2001, rep(2005, 2), rep(NA, 6)),
                src=c(rep("NLCD", 8), rep("CDL", 3), "NLCD", rep("CDL", 4)))
d <- data.frame(d[1:8, ], d[9:16, ])
columns <- rep(c("Year", "Proxy year", "Data source"), 2)
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Applied land cover land use maps during the model simulation period (1995--2010)."
cap2 <- "\\textbf{Abbreviations}: NLCD, National Land Cover Dataset; CDL, Cropland Data Layer; NA, not applicable"
tbl <- xtable::xtable(d, label="table_land-cover-ar")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- "rccc|ccc"
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="NA")

## ----table_et_ratios-ar, echo=FALSE, results="asis"----------------------
d <- data.frame(covers=covers[-c(1, length(covers))],
                nov=c(0.95, 1.02, 1.03, 1.03, 0.95, 1.00, 0.99, 1.10, 1.03, 1.03, 1.05, 1.03, 1.06, 1.02),
                dec=c(1.09, 0.97, 0.93, 1.00, 1.09, 0.90, 1.14, 1.05, 1.00, 1.00, 1.00, 1.00, 1.00, 0.93),
                jan=c(1.00, 0.97, 1.00, 0.92, 1.00, 0.96, 1.24, 0.92, 0.92, 0.92, 0.94, 0.96, 0.92, 0.91),
                feb=c(0.91, 0.90, 0.91, 0.92, 0.91, 0.92, 1.11, 0.92, 0.92, 0.92, 0.88, 0.90, 0.92, 0.91),
                mar=c(0.71, 0.78, 0.77, 0.73, 0.71, 0.72, 0.94, 0.74, 0.71, 0.72, 0.77, 0.73, 0.72, 0.72))
columns <- c("Land cover", "Nov \\\\ (1)", "Dec \\\\ (1)", "Jan \\\\ (1)", "Feb \\\\ (1)", "Mar \\\\ (1)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap <- "Ratio of monthly land cover evapotranspiration rates estimated at the Hailey HADS weather station to those estimated at the Picabo AgriMet weather station."
tbl <- xtable::xtable(d, label="table_et_ratios-ar")
xtable::caption(tbl) <- cap
xtable::digits(tbl) <- 2
xtable::align(tbl) <- c("l", "l", "c", "c", "c", "c", "c")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      size="\\small")

## ----echo=FALSE----------------------------------------------------------
dist <- rgeos::gDistance(weather.stations[weather.stations@data$id == "Ketchum", ],
                         weather.stations[weather.stations@data$id == "Mackay", ])
dist <- sprintf("%s km [%s mi]", formatC(dist * m.to.mi, format="d", big.mark=","),
                 formatC(dist * m.to.km, format="d", big.mark=","))
dz <- abs(weather.stations@data[weather.stations@data$id == "Ketchum", "elevation"] -
          weather.stations@data[weather.stations@data$id == "Mackay", "elevation"])
dz <- sprintf("%s m [%s ft]", formatC(dz, format="d", big.mark=","),
              formatC(dz * m.to.ft, format="d", big.mark=","))

## ----table_et_mackay-ar, echo=FALSE, results="asis"----------------------
d <- data.frame(covers=covers[-c(1, length(covers))],
                nov=c(0.16, 0.33, 0.28, 0.24, 0.16, 0.22, 0.57, 0.22, 0.22, 0.22, 0.29, 0.24, 0.24, 0.28),
                dec=c(0.11, 0.26, 0.26, 0.18, 0.11, 0.18, 0.27, 0.18, 0.18, 0.18, 0.26, 0.18, 0.18, 0.25),
                jan=c(0.14, 0.34, 0.34, 0.23, 0.14, 0.23, 0.32, 0.23, 0.23, 0.23, 0.34, 0.23, 0.23, 0.33),
                feb=c(0.21, 0.45, 0.43, 0.33, 0.21, 0.32, 0.60, 0.33, 0.33, 0.32, 0.44, 0.32, 0.33, 0.47),
                mar=c(0.33, 0.46, 0.41, 0.50, 0.33, 0.40, 1.33, 0.43, 0.43, 0.42, 0.46, 0.50, 0.50, 0.67))
columns <- c("Land cover", "Nov \\\\ (mm/d)", "Dec \\\\ (mm/d)", "Jan \\\\ (mm/d)", "Feb \\\\ (mm/d)", "Mar \\\\ (mm/d)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Average monthly land cover evapotranspiration rates at the Mackay HADS weather station as calculated by the Allen and Robison method."
cap2 <- "\\textbf{Abbreviations}: mm/d, millimeters per day"
tbl <- xtable::xtable(d, label="table_et_mackay-ar")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 2
xtable::align(tbl) <- c("l", "l", "c", "c", "c", "c", "c")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      size="\\small")

## ----table_metric_data, echo=FALSE, results="asis"-----------------------
d <- data.frame(yr=c(1996, 2000, 2002, 2006, 2008:2010),
                mo=c(rep("Apr--Oct", 2), "May--Oct", rep("Apr--Oct", 4)),
                covers=c(rep("WRV", 2), "South of Bellevue", rep("WRV", 4)))
columns <- c("Year", "Month", "Land cover")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap <- "Availability of METRIC estimates of evapotranspiration in the Wood River Valley (WRV) during the model simulation period (1995--2010)."
tbl <- xtable::xtable(d, label="table_metric_data")
xtable::caption(tbl) <- cap
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "l", "l")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small",
      sanitize.text.function=identity)

## ----table_ndvi_data, echo=FALSE, results="asis"-------------------------
d <- data.frame(yr=c(1995, 1997:1999, 2001, 2003:2005, 2007),
                mo=c("July", "Sep--Oct", rep("Aug--Oct", 2), "Jun--Oct",
                     "Jun--Sep", "Apr, Jul, and Oct", rep("Apr--Oct", 2)),
                covers=rep("WRV", 9))
columns <- c("Year", "Month", "Land cover")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap <- "Availability of NDVI estimates of evapotranspiration in the Wood River Valley (WRV) during the model simulation period (1995--2010)."
tbl <- xtable::xtable(d, label="table_ndvi_data")
xtable::caption(tbl) <- cap
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "l", "l")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small",
      sanitize.text.function=identity)

## ----table_intrpl_data, echo=FALSE, results="asis"-----------------------
d <- data.frame(yr=c(1995, 1997:1999, 2001:2004),
                mo=c("Apr--Jun and Aug--Oct", "Apr--Aug", rep("Apr--Jul", 2),
                     "Apr--May", "Apr", "Apr--May and Oct", "May--Jun and Aug--Sep"),
                covers=rep("WRV", 8))
columns <- c("Year", "Month", "Land cover")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap <- "Availability of interpolated estimates of evapotranspiration in the Wood River Valley (WRV) during the model simulation period (1995--2010)."
tbl <- xtable::xtable(d, label="table_intrpl_data")
xtable::caption(tbl) <- cap
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "l", "l")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small",
      sanitize.text.function=identity)

## ----echo=FALSE----------------------------------------------------------
zlim <- range(pretty(c(et[["200802"]][], et[["200808"]][])))
FUN <- function(i) {
  PlotMap(i, xlim=usr.map[1:2], ylim=usr.map[3:4], zlim=zlim,
          bg.image=hill.shading, bg.image.alpha=0.6, dms.tick=TRUE,
          pal=topo.colors, explanation="Evapotranspiration rate, in meters per month.",
          rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit,
          scale.loc="bottomleft")
  plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
  text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
}

## ----map_et_a, echo=FALSE, results="asis", fig.width=fin.map[1], fig.height=fin.map[2]----
FUN(et[["200802"]])

## ----include=FALSE-------------------------------------------------------
v <- "Spatial distribution of evapotranspiration during (\\textit{\\textbf{A}}) February and (\\textit{\\textbf{B}}) August of 2008, Wood River Valley, Idaho. \\label{fig:map_et}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_et_b, echo=FALSE, results="asis", fig.width=fin.map[1], fig.height=fin.map[2]----
FUN(et[["200808"]])

## ----echo=FALSE----------------------------------------------------------
x <- as.Date(paste0(names(et), "15"), format="%Y%m%d")
d <- as.data.frame(do.call(cbind, lapply(seq_len(nlayers(et)), function(i) et[[i]][])))
names(d) <- format(x, "%m/%y")
ylim <- range(pretty(range(cellStats(et, stat="range"))))
pars <- list(boxwex=0.8, staplewex=0.5, outwex=0.5, cex.axis=0.7, las=1)
ylab <- paste("Evapotranspiration rate, in", c("meters", "feet"), "per month")
FUN <- function(xlim) {
  xlim <- as.Date(xlim)
  idxs <-  which(x > xlim[1] & x < xlim[2])
  PlotGraph(x[idxs], d[, idxs], xlim=xlim, type="w", ylab=ylab, ylim=ylim, col="#FFE4C4",
            seq.date.by="year", conversion.factor=m.to.ft, boxwex=20,
            center.date.labels=TRUE)
}

## ----graph_et_a, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("1995-01-01", "1999-01-01"))

## ----include=FALSE-------------------------------------------------------
v <- "Monthly evapotranspiration rate statistics during (\\textit{\\textbf{A}}) 1995--1998, (\\textit{\\textbf{B}}) 1999--2002, (\\textit{\\textbf{C}}) 2003--2006, and (\\textit{\\textbf{D}}) 2007--2010, Wood River Valley, Idaho. \\label{fig:graph_et}"
v <- c(paste("Graphs showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_et_b, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("1999-01-01", "2003-01-01"))

## ----graph_et_c, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("2003-01-01", "2007-01-01"))

## ----graph_et_d, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("2007-01-01", "2011-01-01"))

## ----include=FALSE-------------------------------------------------------
v <- "Spatial distribution of surficial soil types in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_soils, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
Pal <- function(...) {
  cols1 <- rainbow(..., s=0.5, v=1)
  cols2 <- rainbow(..., s=1, v=0.5)
  is.even <- seq_along(cols1) %% 2 == 0
  cols1[is.even] <- cols2[is.even]
  return(cols1)
}
polys <- SetPolygons(soils, alluvium.extent, "gIntersection", 0.001)
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
        bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes),
        credit=credit, scale.loc="bottomleft")
idxs <- as.integer(polys@data$GroupSymbol)
cols <- Pal(max(idxs), alpha=0.9)
plot(polys, col=cols[idxs], border=NA, add=TRUE)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
leg <- as.character(levels(polys@data$GroupSymbol))
legend("topright", leg, fill=cols, border=NA, inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, xpd=NA, bg="#FFFFFFE7", title=expression(bold("Soil Types")))

## ----table_soils, echo=FALSE, results="asis"-----------------------------
d <- soils@data[!is.na(soils@data$SoilClass), ]
d <- d[na.omit(match(levels(d$GroupSymbol), d$GroupSymbol)), ]
d$Rate_ft.per.mo <- d$PercolationRate * m.to.ft
d$MinRate <- ToScientific(d$MinRate, digits=1)
d$MaxRate <- ToScientific(d$MaxRate, digits=1)
d$RateRange <- paste(d$MinRate, "to", d$MaxRate)
d$PercolationRate <- ToScientific(d$PercolationRate, digits=2)
d$Rate_ft.per.mo <- ToScientific(d$Rate_ft.per.mo, digits=2)
d <- d[, c("SoilClass", "GroupSymbol", "RateRange", "PercolationRate", "Rate_ft.per.mo")]
columns <- c("USCS soil class",
             "Symbol",
             "Expected interval for \\\\ percolation rate \\\\ (m/mo)",
             "Percolation \\\\ rate \\\\ (m/mo)",
             "Percolation \\\\ rate \\\\ (ft/mo)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Soil classes and corresponding percolation rates in the Wood River Valley, Idaho."
cap2 <- c("\\textbf{Abbreviations}: m/mo, meters per month; ft/mo, feet per month")
tbl <- xtable::xtable(d, label="table_soils")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::align(tbl)[4] <- "l"
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      sanitize.text.function=identity, size="\\small")

## ----include=FALSE-------------------------------------------------------
v <- "Spatial distribution of percolation in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_percolation, echo=FALSE, fig.width=fin.map[1], fig.height=fin.map[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
r <- rasterize(polys, raster(hill.shading), "PercolationRate")
r[] <- log10(r[])
breaks <- pretty(r[], n=30, na.rm=TRUE)
at <- breaks[c(TRUE, FALSE, FALSE)]
labels <- ToScientific(10^at, digits=1, type="plotmath")
Pal <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))
PlotMap(r, breaks=breaks, xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, bg.image.alpha=0.6, dms.tick=TRUE,
        pal=Pal, explanation="Percolation rate, in meters per month, plotted on a logarithmic scale.",
        rivers=list(x=streams.rivers), lakes=list(x=lakes),
        labels=list(at=at, labels=labels), credit=credit, scale.loc="bottomleft")
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)

## ----echo=FALSE----------------------------------------------------------
rs <- rs.rech.non.irr
zlim <- range(pretty(c(rs[["200802"]][], rs[["200808"]][])))
if (zlim[1] < 0) {
  ratio <- abs(zlim[1]) / diff(zlim)
  Pal <- function(...) {
    Pal1 <- colorRampPalette(c(rep("#F02311", 2), "#FFD0D4"))
    Pal2 <- colorRampPalette(c("#BAE4E5", "#101F78"))
    n1 <- round(... * ratio)
    n2 <- ... - n1
    return(c(Pal1(n1), Pal2(n2)))
  }
} else {
  Pal <- colorRampPalette(c("#FCFBE3", "#67A9CF"))
}
FUN <- function(i) {
  PlotMap(i, xlim=usr.map[1:2], ylim=usr.map[3:4], zlim=zlim,
          bg.image=hill.shading, bg.image.alpha=0.6, dms.tick=TRUE, pal=Pal,
          explanation="Natural recharge rate, in cubic meters per month.",
          rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit,
          scale.loc="bottomleft")
  plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
  text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
}

## ----map_rate_a, echo=FALSE, results="asis", fig.width=fin.map[1], fig.height=fin.map[2]----
FUN(rs[["200802"]])

## ----include=FALSE-------------------------------------------------------
v <- "Natural groundwater recharge during (\\textit{\\textbf{A}}) February and (\\textit{\\textbf{B}}) August of 2008, Wood River Valley, Idaho. \\label{fig:map_rate}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_rate_b, echo=FALSE, results="asis", fig.width=fin.map[1], fig.height=fin.map[2]----
FUN(rs[["200808"]])

## ----echo=FALSE----------------------------------------------------------
x <- as.Date(paste0(names(rs), "15"), format="%Y%m%d")
d <- as.data.frame(do.call(cbind, lapply(seq_len(nlayers(rs)), function(i) rs[[i]][])))
names(d) <- format(x, "%m/%y")
ylim <- range(pretty(range(cellStats(rs, stat="range"))))
pars <- list(boxwex=0.8, staplewex=0.5, outwex=0.5, cex.axis=0.7, las=1)
ylab <- paste("Natural groundwater recharge rate, in cubic", c("meters", "feet"), "per month")
FUN <- function(xlim) {
  xlim <- as.Date(xlim)
  idxs <-  which(x > xlim[1] & x < xlim[2])
  PlotGraph(x[idxs], d[, idxs], xlim=xlim, type="w", ylab=ylab, ylim=ylim, col="#FFE4C4",
            seq.date.by="year", conversion.factor=m.to.ft, boxwex=20,
            center.date.labels=TRUE, scientific=c(FALSE, FALSE, TRUE))
}

## ----graph_rate_a, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("1995-01-01", "1999-01-01"))

## ----include=FALSE-------------------------------------------------------
v <- "Monthly natural groundwater recharge rate statistics during (\\textit{\\textbf{A}}) 1995--1998, (\\textit{\\textbf{B}}) 1999--2002, (\\textit{\\textbf{C}}) 2003--2006, and (\\textit{\\textbf{D}}) 2007--2010, Wood River Valley, Idaho. \\label{fig:graph_rate}"
v <- c(paste("Graphs showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_rate_b, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("1999-01-01", "2003-01-01"))

## ----graph_rate_c, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("2003-01-01", "2007-01-01"))

## ----graph_rate_d, echo=FALSE, results="asis", fig.width=fin.graph.short[1], fig.height=fin.graph.short[2]----
FUN(c("2007-01-01", "2011-01-01"))

