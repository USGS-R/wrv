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
mi2.to.m2  <- 2589990
m3.per.d.to.af.per.yr <- 0.296106669
m2.to.km2 <- 1e-06
mi2.to.km2 <- 2.58999
m3.per.d.to.af.per.y <- 0.2961208
m.to.ft <- 3.28084
mo.to.d <- 30.4368

# Map credit
credit <- paste("Base derived from U.S. Geological Survey National Elevation Dataset 10-meter digital elevation model.",
                "Idaho Transverse Mercator projection; North American Datum of 1983.", sep="\n")

## ----include=FALSE-------------------------------------------------------
v <- "Location of underflow boundaries in the major tributary canyons and the upper part of the Wood River Valley aquifer system, south-central Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_tribs, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(alluvium.thickness), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading, dms.tick=TRUE,
        bg.image.alpha=0.6, rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit,
        scale.loc="bottomleft")
cols <- c("#F02311", "#333333", "#FFFFFFCC", "#FBB829")
plot(alluvium.extent, border=cols[3], col="#33333366", add=TRUE)
l <- rgeos::gIntersection(as(tributaries, "SpatialLinesDataFrame"), alluvium.extent, TRUE)
trib.lines <- SpatialLinesDataFrame(l, data=tributaries@data, match.ID=FALSE)
plot(trib.lines, lwd=4, col=cols[1], add=TRUE)
pos <- c(2, 4, 2, 2, 2, 3, 1, 1, 4, 3,
         4, 1, 1, 1, 2, 4, 2, 1, 1, 3,
         2, 4, 1)
text(getSpatialLinesMidPoints(trib.lines), labels=rownames(trib.lines@data),
     col="#333333", cex=0.6, pos=pos, offset=0.4)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
gage.hailey <- streamgages[streamgages@data$SiteNo == "13139510", ]
points(gage.hailey, pch=17, col=cols[2])
text(gage.hailey, labels=gage.hailey@data$SiteNo, col="#333333", cex=0.6, pos=2)
l <- canals[with(canals@data, grep("Hiawatha", EntityName)), ]
lines(l, col=cols[4])
leg <- c("Flux boundaries", "Hailey streamgage", "Aquifer extent", "Hiawatha Canal")
legend("topleft", leg, pch=c(15, 17, 22, NA), lwd=c(NA, NA, NA, 1), col=cols,
       pt.bg="#33333366", pt.cex=c(0.7, 1, 1.5), inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, bg="#FFFFFFE7", seg.len=1.4)
AddInsetMap(idaho, width=1, main.label=list("IDAHO", adj=c(-0.4, -4.9)),
            sub.label=list("Map area", adj=c(0.5, 2.5)), loc="topright")

## ----table_tribs, echo=FALSE, results="asis"-----------------------------
d <- tributaries@data
d$K <- NULL
d <- data.frame(Name=d$Name, ID=seq_len(nrow(d)), d[, -1])
d$Flow_af <- d$Flow * m3.per.d.to.af.per.yr
d <- d[, c("Name", "ID", "TribWidth", "BdrkDepth", "SatArea", "LandGrad",
           "DarcyFlow", "PrecipRate", "BasinArea", "BasinAreaType",
           "PrecipFlow", "FlowRatio", "Flow", "Flow_af")]
d$PrecipRate <- ToScientific(d$PrecipRate, digits=1)
d$BasinArea  <- ToScientific(d$BasinArea,  digits=1)
columns <- c("Name",
             "Trib \\\\ No.",
             "Canyon \\\\ width \\\\ $w$ \\\\ (m)",
             "Saturated \\\\ thickness \\\\ $\\Delta d$ \\\\ (m)",
             "Saturated \\\\ area \\\\ $A_{sat}$ \\\\ (m\\textsuperscript{2})",
             "Hydraulic \\\\ gradient \\\\ $\\nabla h$ \\\\ (1)",
             "Darcy flow \\\\ (initial  \\\\ estimate) \\\\ $\\overline{Q}_{d}$ \\\\ (m\\textsuperscript{3}/d)",
             "Precip. \\\\ rate \\\\ $P_{b}$ \\\\ (m/d)",
             "Basin \\\\ area \\\\ $A_{b}$ \\\\ (m\\textsuperscript{2})",
             "Basin \\\\ size",
             "Precip. \\\\ flow \\\\ $\\overline{Q}_{p}$ \\\\ (m\\textsuperscript{3}/d)",
             "Flow \\\\ ratio \\\\ $r$ \\\\ (1)",
             "Flow rate \\\\ (secondary  \\\\ estimate) \\\\$\\overline{Q}$ \\\\ (m\\textsuperscript{3}/d)",
             "Flow \\\\ rate \\\\ (acre-ft/yr)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Initial and secondary estimates of mean tributary basin underflow and selected tributary basin information."
cap2 <- c("\\textbf{Trib~No.}: is an identifier used to locate the tributary model boundaries on the map in \\hyperref[fig:map_tribs]{figure~\\ref{fig:map_tribs}}.",
          "\\textbf{Canyon width}: is the width of the tributary canyon floor measured perpendicular to the canyon axis (also known as the `transect line')",
          "\\textbf{Saturated thickness}: is the vertical distance between bedrock and the water table.",
          "\\textbf{Saturated area}: is the saturated cross-sectional area beneath the transect line represented by the lower-half of an ellipse.",
          "\\textbf{Hydraulic gradient}: is the average slope of the water table in the near vicinity of the transect line.",
          "\\textbf{Darcy flow}: is the Darcy estimate of mean tributary basin underflow.",
          "\\textbf{Precip. rate}: is the average precipitation rate in the tributary basin.",
          "\\textbf{Basin area}: is the drainage area of the tributary basin.",
          "\\textbf{Basin size}: is an identifier indicating the size of a tributary basin, where `big' and `small' indicate basin areas greater than and less than 10 square miles, respectively.",
          "\\textbf{Precip. flow}: is the precipitation estimate of mean tributary basin underflow.",
          "\\textbf{Flow ratio}: is the ratio of Darcy to precipitation mean tributary basin underflow.",
          "\\textbf{Flow rate}: is the secondary estimate of mean tributary basin underflow. These flow rates are preliminary and were adjusted during model calibration.",
          "\\textbf{Abbreviations}: m, meters; m\\textsuperscript{2}, square meters; m\\textsuperscript{3}/d, cubic meters per day; m/d, meters per day; acre-ft/yr, acre-feet per year")
tbl <- xtable::xtable(d, label="table_tribs")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- c(0, 0, 0, 0, 1, 0, 4, 0, 1, 1, 0, 0, 3, 0, 0)
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      sanitize.text.function=identity, size="\\small")

## ----echo=FALSE----------------------------------------------------------
# Adapted from plotrix::draw.ellipse function (v3.5-11)
DrawEllipse <- function(x, y, a, b, ang=0, seg=c(0, 360), nv=1000) {
  theta <- ang * pi / 180
  seg <- seg * pi / 180
  z <- seq(seg[1], seg[2], length=nv + 1L)
  xx <- a * cos(z)
  yy <- b * sin(z)
  alpha <- atan2(yy, xx)
  rad <- sqrt(xx^2 + yy^2)
  xp <- rad * cos(alpha + theta) + x
  yp <- rad * sin(alpha + theta) + y
  return(xy.coords(xp, yp))
}

RotateLine <- function(m, ang=90) {
  x <- m[, 1]
  y <- m[, 2]
  theta <- ang * pi / 180
  cx <- mean(x)
  cy <- mean(y)
  x.new <- ( (x - cx) * cos(theta) + (y - cy) * sin(theta)) + cx
  y.new <- (-(x - cx) * sin(theta) + (y - cy) * cos(theta)) + cy
  return(cbind(x=x.new, y=y.new))
}

## ----map_graph_trib_a, echo=FALSE, results="asis", fig.width=fin.map.n.small.0[1], fig.height=fin.map.n.small.0[2]----
trib.name <- "Deer Creek"
transect <- trib.lines[trib.lines@data$Name == trib.name, ]
xy <- as.numeric(coordinates(getSpatialLinesMidPoints(transect)))
dx <- 2000
xy[1] <- xy[1] + 700
e <- extent(c(xy[1] - dx, xy[1] + dx, xy[2] - dx, xy[2] + dx))
PlotMap(crs(alluvium.thickness), xlim=e[1:2], ylim=e[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.8,
        rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=NULL,
        scale.loc="bottomleft")
cols <- c("#F02311", "#FFC800", "#333333CC")
plot(transect, lwd=2, col=cols[1], add=TRUE)
pts <- as(transect, "SpatialPoints")
text(pts, c("B", "B'"),  cex=0.7, pos=c(3, 1), font=4)
lines(RotateLine(coordinates(pts)), lwd=2, col=cols[2])
plot(alluvium.extent, border=cols[3], add=TRUE)
leg <- c("Transect line", "Axial line", "Aquifer extent")
legend("topleft", leg, lwd=c(2, 2, 1), col=cols, pt.bg="#33333366",
       inset=0.02, cex=0.7, box.lty=1, box.lwd=0.3, bg="#FFFFFFE7",
       seg.len=1)

## ----map_graph_trib_b, echo=FALSE, results="asis", fig.width=fin.map.n.small.0[1], fig.height=fin.map.n.small.0[2]----
asp <- 10
sat.thickness <- transect@data$BdrkDepth
depth.to.water <- sat.thickness * 0.1  # estimating at 10 percent of saturated thickness
land.elev <- transect@data$MinLSD + depth.to.water
xy <- coordinates(transect)[[1]][[1]]
xlim <- c(0, as.numeric(dist(xy)))
ylim <- c(land.elev - depth.to.water - sat.thickness, land.elev)
par(mar=c(3.0, 0.1, 1.1, 0.1))
plot.new()
plot.window(xlim=range(extendrange(xlim)), ylim=extendrange(ylim),
            asp=asp, xaxt="n", yaxt="n", xaxs="i", yaxs="i")
usr <- par("usr")
leg <- c("Unsaturated zone", "Saturated zone", "Bedrock")
cols <- c("#BF5507", "#3399CC", "#C0C0C0")
polygon(c(usr[1:2], rev(usr[1:2])), c(rep(land.elev, 2), rep(usr[3], 2)),
        border=NA, col=cols[3])
p <- DrawEllipse(mean(xlim), land.elev - depth.to.water,
                 diff(xlim) / 2, sat.thickness, seg=c(180, 360))
polygon(p, border=NA, col=cols[2], lty=1, lwd=1)
polygon(c(xlim, rev(xlim)), c(rep(land.elev, 2), rep(land.elev, 2) - depth.to.water),
        border=NA, col=cols[1])
AddScaleBar(unit="meters", vert.exag=asp, loc="bottom", inset=0.12)
legend("bottom", leg, fill=cols, horiz=TRUE, inset=-0.15,
       cex=0.7, bty="n", xpd=TRUE, xjust=0.5, yjust=0, border=NA)
text(xlim[1], ylim[2], "B",  cex=0.7, pos=3, font=4)
text(xlim[2], ylim[2], "B'", cex=0.7, pos=3, font=4)
text(xlim[2], ylim[2], "B'", cex=0.7, pos=3, font=4)
text(mean(xlim), ylim[2], "LAND SURFACE", cex=0.7, pos=3)

## ----include=FALSE-------------------------------------------------------
v <- "Basin area in ascending order plotted on a logarithmic scale."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_basin_areas, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
natural.basin.area.break <- 10 * mi2.to.km2  # 10 square-miles
d <- tributaries@data
d$BasinArea <- d$BasinArea * m2.to.km2
ylim <- 10^(range(pretty(log10(d$BasinArea))))
x <- d[order(d$BasinArea), c("Name", "BasinArea")]
ylab <- paste("Basin area, in square", c("kilometers", "miles"))
PlotGraph(x, ylab=ylab, ylim=ylim, ylog=TRUE, type="p", pch=21, pt.cex=0.7,
          col="#2A8FBD", bg="#2A8FBD", conversion.factor=1 / mi2.to.km2,
          xlab="Rank ordered plotting position")
col <- "#A80000"
abline(h=natural.basin.area.break, col=col)
pos <- rep(4, nrow(x))
pos[21:23] <- 2
text(seq_along(x[, 1]), x[, 2], x[, 1], pos=pos, offset=0.3, cex=0.6, srt=90)
xlim <- par("usr")[1:2]
text(xlim[2] - diff(xlim) * 0.15, natural.basin.area.break,
     "Natural break in basin areas", col=col, cex=0.7, pos=1, offset=0.2)

## ----hist_flux_est, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.cap="{Estimated mean tributary basin underflow in the Wood River Valley aquifer system, south-central Idaho. Values are preliminary and were adjusted during model calibration.}"----
d <- tributaries@data[order(tributaries@data$Flow, decreasing=TRUE), ]
cols <- c("#345381", "#E33258")
col <- rep(cols[1], nrow(d))
col[d$BasinAreaType == "small"] <- cols[2]
par(mar=c(3.1, 7.1, 0.1, 1.1), mgp=c(1.5, 0.1, 0.3))
xlab <- "Estimated mean volumetric flow rate, in cubic meters per day"
at <- pretty(d$Flow, n=8)
barplot(d$Flow, space=0.4, xlim=range(at), names.arg=d$Name, horiz=TRUE, cex.names=0.7,
        cex.lab=0.7, las=1, xlab=xlab, axes=FALSE, lwd=0.5, border=NA, col=col)
tcl <- 7.2 / par("cra")[2]
labs <- formatC(at, big.mark=",", mode="integer")
axis(1, at=at, labels=labs, cex.axis=0.7, tcl=tcl, lwd=0.5)
labs <- paste("Basin size is", c("'big'", "'small'"))
legend("topright", labs, fill=cols, border=cols, inset=0.05, cex=0.7, pt.cex=1,
       box.lwd=NA, xpd=NA, bg="#FFFFFFE7")

## ----include=FALSE-------------------------------------------------------
v <- "Daily streamflow at the Hailey (13139510) streamgage located on the Big Wood River, Idaho."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_disch_hailey, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- gage.disch[, c("Date", "13139510")]
d <- d[order(d$Date), ]
xlim <- as.Date(c("1994-01-01", "2011-01-01"))
ylab <- paste("Streamflow, in", c("cubic meters per day", "acre-foot per year"))
PlotGraph(d, ylab = ylab, xlim=xlim, col="#3399CC", lty=1,
          conversion.factor=m3.per.d.to.af.per.y, center.date.labels=TRUE,
          scientific=c(FALSE, TRUE, TRUE), seq.date.by="year")

## ----echo=FALSE----------------------------------------------------------
ave.flow <- tributaries$Flow[tributaries$Name == "Deer Creek"]
tr.interval <- as.Date(c("1995-01-01", "2011-01-01"), tz="MST")
tr.stress.periods <- seq(tr.interval[1], tr.interval[2], "1 month")
disch <- gage.disch[, c("Date", "13139510")]

mo <- c(1, 9)
dt <- mo * mo.to.d
l <- lapply(dt, function(i) GetSeasonalMult(disch, 1, i, tr.stress.periods))
d <- do.call("cbind", l)
d <- d[, -which(colnames(d) == "Date")[-1]]
d[, -1] <- d[, -1] * ave.flow
colnames(d) <- c("Date", paste0("mo.", mo))
d.dt <- d

rf <- c(1, 2)
l <- lapply(rf, function(i) GetSeasonalMult(disch, i, dt[2], tr.stress.periods))
d <- do.call("cbind", l)
d <- d[, -which(colnames(d) == "Date")[-1]]
d[, -1] <- d[, -1] * ave.flow
colnames(d) <- c("Date", paste0("rf.", rf))
d.rf <- d

av <- c(1.2, 1.0, 0.8)
d <- GetSeasonalMult(disch, rf[2], dt[2], tr.stress.periods)
mult <- d[, 2]
d[, 2] <- mult * ave.flow * av[1]
d[, 3] <- mult * ave.flow * av[2]
d[, 4] <- mult * ave.flow * av[3]
colnames(d) <- c("Date", paste0("av.", av))
d.av <- d

## ----include=FALSE-------------------------------------------------------
v <- "Seasonally adjusted tributary basin underflow in the Deer Creek tributary canyon calculated using a 1- and 9-month sampling window, while holding constant the reduction factor at 1 and estimated long-term average underflow at 4,937 cubic meters per day."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_flow_dt, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
ylab <- paste("Tributary basin underflow, in", c("cubic meters per day", "acre-feet per year"))
labs <- paste0(mo, "-month sampling window")
cols <- c("#00C176", "#1F1F1F")
d <- rbind(d.dt, d.dt[nrow(d.dt), , drop=FALSE])
d[nrow(d), "Date"] <- d[nrow(d), "Date"] + diff(d$Date)[1]
PlotGraph(d, ylab=ylab, col=cols, fill="tozeroy", conversion.factor=m3.per.d.to.af.per.yr,
          center.date.labels=TRUE, scientific=c(FALSE, FALSE, FALSE),
          seq.date.by="year")
legend("topright", labs, col=cols, lty=1, inset=0.02, cex=0.7,
       box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")

## ----include=FALSE-------------------------------------------------------
v <- "Seasonally adjusted tributary basin underflow in the Deer Creek tributary canyon calculated using a reduction factor of 1 and 2, while holding constant the sampling window at 9 months and estimated long-term average underflow at 4,937 cubic meters per day."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_flow_rf, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
labs <- paste("Reduction factor of", rf)
cols <- c("#F02311", "#1F1F1F")
d <- rbind(d.rf, d.rf[nrow(d.rf), , drop=FALSE])
d[nrow(d), "Date"] <- d[nrow(d), "Date"] + diff(d$Date)[1]
PlotGraph(d, ylab=ylab, col=cols, fill="tozeroy", conversion.factor=m3.per.d.to.af.per.yr,
          center.date.labels=TRUE, scientific=c(FALSE, FALSE, FALSE),
          seq.date.by="year")
legend("topright", labs, col=cols, lty=1, inset=0.02, cex=0.7,
       box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")

## ----include=FALSE-------------------------------------------------------
v <- "Seasonally adjusted tributary basin underflow in the Deer Creek tributary canyon calculated using 120, 100, and 80 percent of the estimated long-term average underflow (4,937 cubic meters per day). The sampling window and reduction factor were held constant at 9 months and 2, respectively."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_flow_av, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
labs <- paste(av * 100, "percent")
cols <- c("#02779E", "#1F1F1F", "#FFCC00")
d <- rbind(d.av, d.av[nrow(d.av), , drop=FALSE])
d[nrow(d), "Date"] <- d[nrow(d), "Date"] + diff(d$Date)[1]
PlotGraph(d, ylab=ylab, col=cols, fill="tozeroy", conversion.factor=m3.per.d.to.af.per.yr,
          center.date.labels=TRUE, scientific=c(FALSE, FALSE, FALSE),
          seq.date.by="year")
legend("topright", labs, col=cols, lty=1, inset=0.02, cex=0.7,
       box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")

