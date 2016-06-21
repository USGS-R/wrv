## ----setup, include=FALSE------------------------------------------------
t0 <- Sys.time()
try(knitr::opts_chunk$set(tidy=FALSE, comment="#", fig.align="center"), silent=TRUE)
grDevices::pdf.options(useDingbats=FALSE)
options(preferRaster=TRUE, scipen=0, digits=2)
library(wrv)
loadNamespace("xtable")

# Device dimension in inches (width, height)
fin.graph         <- c(7.16, 7.16)
fin.graph.short   <- c(7.16, 3.50)
fin.map           <- c(7.01, 9.32)
fin.map.0         <- c(7.01, 8.65)
fin.map.s         <- c(7.16, 5.31)
fin.map.s.0       <- c(7.16, 4.64)
fin.map.n         <- c(7.16, 7.00)
fin.map.n.small   <- c(3.50, 3.83)
fin.map.n.small.0 <- c(3.50, 3.16)
fin.cs            <- c(7.16, 5.39)
fin.cs.0          <- c(7.16, 4.73)

# Extreme coordinates of plotting region (x1, x2, y1, y2)
usr.map     <- c(2451504, 2497815, 1342484, 1402354)
usr.map.s   <- c(2472304, 2497015, 1343284, 1358838)
usr.map.n.1 <- c(2463000, 2475356, 1386500, 1398856)
usr.map.n.2 <- c(2467500, 2479856, 1376500, 1388856)
usr.map.n.3 <- c(2466696, 2479052, 1366501, 1378857)
usr.map.n.4 <- c(2471500, 2483856, 1356482, 1368838)

# Unit conversions
cfs.to.m3.per.d <- 2446.57555

# Map credit
credit <- paste("Base derived from U.S. Geological Survey National Elevation Dataset 10-meter digital elevation model.",
                "Idaho Transverse Mercator projection; North American Datum of 1983.", sep="\n")

## ----echo=FALSE----------------------------------------------------------
is.obs <- as.Date(obs.wells.head$DateTime) >= as.Date("1998-01-01") &
          as.Date(obs.wells.head$DateTime) <  as.Date("2011-01-01")
wells <- obs.wells

FUN <- function(x) {
  x <- range(x)
  moyr <- format(x, "%b %Y")
  mo   <- format(x, "%b")
  yr   <- format(x, "%Y")
  if (moyr[1] == moyr[2]) {
    return(moyr[1])
  } else {
    if (yr[1] == yr[2])
      return(paste0(mo[1],"--",  moyr[2]))
    else
      return(paste(moyr, collapse=" -- "))
  }
}
x <- aggregate(obs.wells.head$DateTime, by=list(obs.wells.head$PESTNAME), FUN)
names(x) <- c("PESTNAME", "por")
d <- dplyr::left_join(obs.wells@data, x, by="PESTNAME")
d$nrec <- vapply(d$PESTNAME, function(i) sum(obs.wells.head$PESTNAME == i), 0L)
x <- aggregate(obs.wells.head$Head, by=list(obs.wells.head$PESTNAME), sd)
names(x) <- c("PESTNAME", "sd")
d <- dplyr::left_join(d, x, by="PESTNAME")
wells@data <- d

PrintTable <- function(idxs, is.last.hline, label) {
  d <- wells@data[idxs[idxs <= nrow(wells@data)], ]
  d$desc <- as.character(d$desc)

  d$desc[d$desc == "Observation well"] <- "USGS well"
  d$desc[d$desc == "Geo-located driller well"] <- "Geo-located driller well"
  d$desc[d$desc == "Driller-located driller well"] <- "PLSS-located driller well"
  d$desc[d$desc == "Sun Valley Water and Sewer well"] <- "SVWSD well"
  d$desc[d$desc == "Nature Conservancy well"] <- "TNC well"

  d[duplicated(d[, "desc"]), "desc"] <- ""
  d$SiteNo[is.na(d$SiteNo)] <- "--"
  d <- d[, c("desc", "id", "SiteNo", "WELLNUMBER", "nrec", "por", "sd")]
  columns <- c("Well type",
               "Well \\\\ No.",
               "Site \\\\ identifier",
               "Name",
               "No. of \\\\ records",
               "Period of \\\\ record",
               "SD \\\\ (m)")
  colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
  cap1 <- "Observation wells in the Wood River Valley aquifer system."
  cap2 <- c("\\textbf{Well type}:",
            "``USGS well'' is monitored by the U.S. Geological Survey and (or) the Idaho Department of Water Resources;",
            "``Geolocated driller well'' is a driller recorded groundwater level in a geolocated well;",
            "``PLSS-located driller well'' is a driller recorded groundwater level in a well located using the Public Land Survey System (PLSS);",
            "``SVWSD well'' is a production well in the Sun Valley Water and Sewer District (SVWSD); and",
            "``TNC well'' is monitored by The Nature Conservancy and installed with a pressure transducer.",
            "\\textbf{Well No.}: identifier used to locate wells in figures \\ref{fig:map_wells_usgs}, \\ref{fig:map_wells_driller}, and \\ref{fig:map_wells_svws_nat}.",
            "\\textbf{Site identifier}: unique numerical identifiers used to access well data (\\url{http://waterdata.usgs.gov/nwis}).",
            "\\textbf{Name}: local well name used in this study.",
            "\\textbf{SD}: standard deviation of groundwater levels.",
            "\\textbf{Abbreviations}: m, meters; --, not available; NA, not applicable")
  tbl <- xtable::xtable(d, label=label)
  if (idxs[1] == 1L)
    xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
  else
    xtable::caption(tbl) <- sprintf("%s---Continued", cap1)
  xtable::digits(tbl) <- c(0, 0, 0, 0, 0, 0, 0, 2)
  suppressWarnings(xtable::align(tbl) <- "llccL{1.0in}rcr")
  hline.after <- if (is.last.hline) c(-1, 0, nrow(tbl)) else c(-1, 0)
  print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
        sanitize.colnames.function=function(x){x}, size="\\small", NA.string="NA",
        sanitize.text.function=identity, hline.after=hline.after)
}

## ----table_wells_usgs, echo=FALSE, results="asis"------------------------
PrintTable(idxs <- 1:(n <- 48L), FALSE, label <- "table_wells_usgs")

## ----echo=FALSE, results="asis"------------------------------------------
s <- unique(c(seq(n, nrow(wells@data), 56L), nrow(wells@data)))
for (i in head(seq_along(s), -1)) {
  cat("\\addtocounter{table}{-1}\n")
  PrintTable((s[i] + 1L):s[i + 1], s[i + 1L] == tail(s, 1), paste0(label, "_", i))
}

## ----include=FALSE-------------------------------------------------------
v <- "Location of wells in the U.S. Geological Survey (USGS) groundwater monitoring network, Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_wells_usgs, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6,
        rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col=NA, add=TRUE)
p <- obs.wells[obs.wells@data$desc == "Observation well", ]
col <- "#FF4B12"
bg  <- "#FF4B12"
points(p, pch=21, cex=0.7, col=col, bg=bg, lwd=0.25)
pos <- c(2, 4, 1, 1, 1, 3, 4, 1, 1, 1,
         3, 3, 4, 1, 1, 2, 3, 3, 3, 4,
         1, 1, 2, 4, 1, 3, 1, 1, 1, 3,
         1, 1, 1, 1, 1, 4, 2, 2, 1, 3,
         3, 4, 2, 4, 2, 2, 2, 3, 2, 4,
         1, 3, 4, 3, 3, 4, 2, 3, 4, 2,
         4, 2, 1, 4, 4, 4, 2, 2, 3, 4,
         2, 4, 2, 4, 1, 2, 4, 1, 1, 4,
         2, 4, 2, 4, 2, 2, 1, 3, 2, 4,
         3, 4, 3, 2)
text(p, labels=p@data$id, col="#333333", cex=0.6, pos=pos, offset=0.3)
leg <- "USGS well"
legend("topright", leg, pch=21, col=col, pt.bg=bg, pt.lwd=0.25, pt.cex=0.8,
       inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")
AddInsetMap(idaho, width=1, main.label=list("IDAHO", adj=c(-0.4, -4.9)),
            sub.label=list("Map area", adj=c(0.5, 2.5)), loc="topleft")
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "Location of geolocated driller wells and Public Land Survey System (PLSS)-located driller wells in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_wells_driller, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6,
        rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col=NA, add=TRUE)
p.1 <- obs.wells[obs.wells@data$desc == "Geo-located driller well", ]
p.2 <- obs.wells[obs.wells@data$desc == "Driller-located driller well", ]
col <- c("#9061C2DD", "#FFC800DD")
bg  <- c("#9061C280", "#FFC80080")
pch <- c(21, 22)
cex <- c(0.7, 0.6)
points(p.1, pch=pch[1], cex=cex[1], col=col[1], bg=bg[1], lwd=0.25)
points(p.2, pch=pch[2], cex=cex[2], col=col[2], bg=bg[2], lwd=0.25)
no.1 <- paste0("(", paste(range(p.1@data$id), collapse="-"), ")")
no.2 <- paste0("(", paste(range(p.2@data$id), collapse="-"), ")")
leg <- c(paste("Geolocated driller well", no.1),
         paste("PLSS-located driller well", no.2))
legend("topright", leg, pch=pch, col=col, pt.bg=bg, pt.cex=cex,
       pt.lwd=0.25, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5,
       bg="#FFFFFFE7")
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "Location of two Sun Valley Water and Sewer District (SVWSD) production wells, and wells in The Nature Conservancy (TNC) groundwater monitoring network, Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_wells_svws_nat, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6,
        rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col=NA, add=TRUE)
p.1 <- obs.wells[obs.wells@data$desc == "Sun Valley Water and Sewer well", ]
p.2 <- obs.wells[obs.wells@data$desc == "Nature Conservancy well", ]
col <- c("#A40802", "#1F78B4")
pch <- c(21, 22)
cex <- c(0.7, 0.7)
points(p.1, pch=pch[1], cex=cex[1], col=col[1], bg=col[1], lwd=0.25)
points(p.2, pch=pch[2], cex=cex[2], col=col[2], bg=col[2], lwd=0.25)
pos <- c(4, 4)
text(p.1, labels=p.1@data$id, col="#333333", cex=0.6, pos=pos, offset=0.3)
pos <- c(1, 1, 1, 4, 1, 1, 1, 1, 4, 1)
text(p.2, labels=p.2@data$id, col="#333333", cex=0.6, pos=pos, offset=0.3)
leg <- c("SVWSD well", "TNC well")
legend("topright", leg, pch=pch, col=col, pt.bg=col, pt.cex=cex,
       pt.lwd=0.25, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "River network and streamflow measurement sites in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_sites, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
sites <- seepage.study
sites <- sites[!sites@data$Type %in% c("Return", "Diversion", "Exchange well inflow"), ]
sites <- sites[!duplicated(sites@data$Name), ]
sites@data$site.no <- seq_len(nrow(sites@data))

PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6, credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col="#FFFFFF9A", add=TRUE)
plot(river.reaches, col="#3399CC", lwd=0.5, add=TRUE)
plot(tributary.streams, col="#3399CC", lwd=0.5, add=TRUE)
is.continuous <- sites@data$SiteNo %in% streamgages@data$SiteNo
cols <- rep("#FDFDFD", length(is.continuous))
cols[is.continuous] <- "#A40802"
points(sites, pch=24, cex=0.8, col="#A40802", bg=cols)
pos <- c(2, 4, 3, 3, 1, 1, 3, 1, 1, 4,
         3, 3, 1, 3, 3, 3, 4, 2, 2, 3,
         1, 1, 3, 1, 3, 1, 3, 1, 3, 1,
         3, 3)
text(sites, labels=sites@data$site.no, col="#333333", cex=0.6, pos=pos, offset=0.3)

lab <- paste0(max(sites@data$site.no), "\n")
text(par("usr")[2], par("usr")[3], labels=lab, col="#333333", cex=0.6, pos=2,
     offset=0.1)
text(2478800, 1355000, labels="Big Wood River", cex=0.6, col="#3399CC", font=3, srt=75)
text(2493200, 1346300, labels="Silver\nCreek", cex=0.6, col="#3399CC", font=3, srt=0)
text(2478500, 1347700, labels="Willow Creek", cex=0.6, col="#3399CC", font=3, srt=15)
text(2483000, 1351600, labels="spring-fed\ntributaries", cex=0.6, col="#3399CC", font=3)
leg <- c("Rivers and streams", "Streamgage with continuous record",
         "Measurement station without a gage")
legend("topright", leg, pch=c(NA, 24, 24), lwd=c(1, NA, NA),
       col=c("#3399CC", "#A40802", "#A40802"), pt.bg=c(NA, "#A40802", "#FDFDFD"),
       pt.cex=0.8, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")

invisible(dev.off())

## ----echo=FALSE----------------------------------------------------------
GetSiteNo <- function(i) {
  sprintf("site No.~%s", sites@data$site.no[sites@data$SiteNo %in% i])
}

## ----table_sites, echo=FALSE, results="asis"-----------------------------
d <- sites@data
d$POR <- "Aug, Oct 2012; Mar 2013"
d$POR[d$SiteNo == 432352114161500] <- "March 2013"
d$POR[d$SiteNo == 432248114163400] <- "March 2013"
d$POR[d$SiteNo == 434404114215200] <- "Aug, Oct 2012"
d$POR[d$SiteNo == 432805114113800] <- "Aug, Oct 2012"
d$POR[d$SiteNo == 13135510]        <- "Aug, Oct 2012"
d$POR[d$SiteNo == 13137900]        <- "Aug, Oct 2012"
d$POR[d$SiteNo == 13135500]        <- "1948--1972, 2011--present"
d$POR[d$SiteNo == 13135520]        <- "2011--present"
d$POR[d$SiteNo == 13137000]        <- "1920--1921, 2011--present"
d$POR[d$SiteNo == 13137500]        <- "1920--1921, 2011--present"
d$POR[d$SiteNo == 13138000]        <- "1920--1921, 2011--present"
d$POR[d$SiteNo == 13139510]        <- "1915--present"
d$POR[d$SiteNo == 13140800]        <- "1996--present"
d$POR[d$SiteNo == 13140900]        <- "1999--present"
d$POR[d$SiteNo == 13150430]        <- "1974--present"
d <- d[, c("site.no", "SiteNo", "Name", "POR")]
columns <- c("Site \\\\ No.", "Site \\\\ identifier", "Name", "Period of record")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Streamflow measurement sites located on the Big Wood River, Willow Creek, Silver Creek, and spring-fed tributaries."
cap2 <- c("\\textbf{Site No.}: identifier used to locate measurement sites on maps located in figures and as a cross reference with data in other tables.",
          "\\textbf{Site identifier}: unique numerical identifiers used to access streamflow data (\\url{http://waterdata.usgs.gov/nwis}).",
          "\\textbf{Name}: local measurement site name used in this study.")
tbl <- xtable::xtable(d, label="table_sites")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "c", "p{3.7in}", "l")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="NA")

## ----table_lm, echo=FALSE, results="asis"--------------------------------
ids <- c(13135500, 13135520, 13137000, 13137500, 13138000)
d <- sites@data[match(ids, sites@data$SiteNo), c("Name", "site.no")]
d$Period <- c("Jun 1948 -- Sep 1971, May 2011 -- Sep 2013",
              "May 2011 -- Sep 2013", "Feb 2011 -- Mar 2014",
              "Dec 2010 -- Mar 2014", "Nov 2011 -- Sep 2013")
d$Slope     <- c(0.34, 0.16, 0.17, 0.12, 0.12)
d$Intercept <- c(0.05, -5.74, 6.59, -11.90, -7.21) * cfs.to.m3.per.d
d$R.squared <- c(0.975, 0.863, 0.885, 0.865, 0.877)
columns <- c("Station name", "Site \\\\ No.", "Coinciding \\\\ time period",
             "Slope \\\\ (1)", "Intercept \\\\ (m\\textsuperscript{3}/d)",
             "R\\textsuperscript{2} \\\\ (1)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- paste("Characteristics of the linear regression models used to estimate streamflow at various streamgages during periods of missing data.",
              "The independent variable of the linear regression models is streamflow measured at the Big Wood River, Hailey streamgage (13139510), in cubic meters per day.")
cap2 <- c("\\textbf{Station name}: local streamgage name used in this study.",
          "\\textbf{Site No.}: identifier used to locate streamflow measurement sites in \\hyperref[fig:map_sites]{figure~\\ref{fig:map_sites}} and \\hyperref[table_sites]{table~\\ref{table_sites}}.",
          "\\textbf{Coinciding time period}: when streamflow data were available at both streamgages.",
          "\\textbf{Slope} and \\textbf{Intercept}: of the linear regression model.",
          "\\textbf{R\\textsuperscript{2}}: the coefficient of determination.",
          "\\textbf{Abbreviations}: m\\textsuperscript{3}/d, cubic meters per day")

tbl <- xtable::xtable(d, label="table_lm")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl)[5:7] <- c(2, 0, 3)
xtable::align(tbl) <- c("l", "p{2.8in}", "c", "p{1.2in}", "r", "r", "r")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="--",
      sanitize.text.function=identity, format.args=list(big.mark=","))

## ----echo=FALSE----------------------------------------------------------

# Apply hack for records in seepage.study that are missing from div.ret.exch
is.missing <- with(seepage.study@data, Type %in% c("Diversion", "Return", "Exchange well inflow") & !Name %in% div.ret.exch$Name)
missing.rec <- seepage.study[is.missing, c("Name", "Type"), drop=FALSE]
missing.rec@data$LocSource <- NA
missing.rec@data$BigReach  <- NA
div.ret.exch <- rbind(div.ret.exch, missing.rec)

p.ret <- div.ret.exch[div.ret.exch@data$Type %in% c("Return", "Exchange well inflow"), ]
p.ret@data$ret.no <- seq_along(p.ret)

GetRetNo <- function(i) {
  return(sprintf("return No.~%s", paste(p.ret@data$ret.no[p.ret$Name %in% i], collapse=",~")))
}

## ----include=FALSE-------------------------------------------------------
v <- "Streamflow returns from irrigation canals or ponds, and exchange wells located on the Big Wood River, Silver Creek, and spring-fed tributaries."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_ret, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6, credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col="#FFFFFF9A", add=TRUE)
plot(river.reaches, col="#3399CC", lwd=0.5, add=TRUE)
plot(tributary.streams, col="#3399CC", lwd=0.5, add=TRUE)
p <- p.ret[p.ret@data$Type == "Return", ]
cols <- c("#F3077D", "#7FAF1B")
points(p, pch=23, cex=0.7, col=cols[1], bg=cols[1])
pos <- c(2, 4, 1, 2, 4, 3, 4)
text(p, labels=p@data$ret.no, col="#333333", cex=0.6, pos=pos, offset=0.3)
p <- p.ret[p.ret@data$Type == "Exchange well inflow", ]
points(p, pch=21, cex=0.7, col=cols[2], bg=cols[2])
pos <- c(2, 3, 4, 1, 2, 3, 3, 1, 3, 1,
         1, 2, 3, 2)
text(p, labels=p@data$ret.no, col="#333333", cex=0.6, pos=pos, offset=0.3)
leg <- c("Irrigation canal or pond return", "Exchange-well return")
legend("topright", leg, pch=c(23, 21), lwd=NA, col=cols, pt.bg=cols,
       pt.cex=0.7, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")
invisible(dev.off())

## ----table_ret, echo=FALSE, results="asis"-------------------------------
d <- p.ret@data
d$Type <- as.character(d$Type)
d$Type[d$Type == "Return"] <- "Irrigation canal or pond"
d$Type[d$Type == "Exchange well inflow"] <- "Exchange well"
d <- d[, c("ret.no", "Name", "Type")]
columns <- c("Return \\\\ No.", "Name", "Water-source")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Streamflow returns located on the Big Wood River, Silver Creek, and spring-fed tributaries."
cap2 <- c("\\textbf{Return No.}: identifier used to locate returns in \\hyperref[fig:map_ret]{figure~\\ref{fig:map_ret}}.",
          "\\textbf{Name}: local name used to identify the return-flow location in this study.",
          "\\textbf{Water-source}: the water type for return flows.")
tbl <- xtable::xtable(d, label="table_ret")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "p{2.0in}", "l")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="--")

## ----include=FALSE-------------------------------------------------------
v <- "Streamflow diversions along on the Big Wood River, Willow Creek, Silver Creek, and spring-fed tributary streams."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_div, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
p.div <- div.ret.exch[div.ret.exch@data$Type == "Diversion", ]
p.div@data$div.no <- seq_along(p.div)
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6, credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col="#FFFFFF9A", add=TRUE)
plot(river.reaches, col="#3399CC", lwd=0.5, add=TRUE)
plot(tributary.streams, col="#3399CC", lwd=0.5, add=TRUE)
points(p.div, pch=22, cex=0.7, col="#F98C64", bg="#F98C64")
lab <- formatC(p.div@data$div.no)
lab[31] <- "31-54"
lab[32:54] <- ""
pos <- c(3, 4, 1, 2, 4, 1, 2, 2, 1, 4,
         4, 3, 4, 3, 1, 4, 1, 2, 1, 4,
         4, 2, 2, 4, 4, 1, 2, 3, 1, 1,
         2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 1, 1, 4, 1, 3, 2,
         2, 4, 1, 2, 4, 2, 2, 4, 2, 4,
         2, 4, 3, 4, 4, 3, 1, 1, 1, 3,
         2, 4, 1, 1, 3, 1, 2, 4, 3, 4,
         2, 4, 1, 2, 3, 1, 3, 4, 1, 1,
         3, 4, 2, 2)
text(p.div, labels=lab, col="#333333", cex=0.6, pos=pos, offset=0.3)
leg <- "Stream diversion"
legend("topright", leg, pch=22, lwd=NA, col="#F98C64", pt.bg="#F98C64",
       pt.cex=0.7, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")
invisible(dev.off())

## ----table_div, echo=FALSE, results="asis"-------------------------------
d <- p.div@data
d$SiteNo <- seepage.study@data$SiteNo[match(d$Name, seepage.study@data$Name)]
d <- d[, c("div.no", "SiteNo", "Name")]
columns <- c("Div. \\\\ No.", "Site \\\\ identifier", "Name")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Streamflow diversions located on the Big Wood River, Willow Creek, Silver Creek, and spring-fed tributaries."
cap2 <- c("\\textbf{Div. No.}: identifier used to locate diversions in \\hyperref[fig:map_div]{figure~\\ref{fig:map_div}}.",
          "\\textbf{Site identifier}: unique numerical identifiers used to access streamflow measurement data (\\url{http://waterdata.usgs.gov/nwis}).",
          "\\textbf{Name}: local diversion name used in this study.",
          "\\textbf{Abbreviations}: --, not available")
idxs <- 1:52
tbl <- xtable::xtable(d[idxs, ], label="table_div")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- "lccl"
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="--")

## ----echo=FALSE, results="asis"------------------------------------------
tbl <- xtable::xtable(d[seq_len(nrow(d))[-idxs], ], label="table_div_1")
 xtable::caption(tbl) <- sprintf("%s---Continued", cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- "lccl"
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="--")

## ----echo=FALSE----------------------------------------------------------
rr <- river.reaches[river.reaches@data$BigReach != "None", ]
rr <- rr[order(rr@data$ReachNo), ]
reach.names <- unique(rr@data$BigReach)
reach.numbers <- match(rr@data$BigReach, reach.names)

GetReachNo <- function(i) {
  d <- data.frame(ReachNo=seq_along(reach.names), Name=as.character(reach.names))
  return(sprintf("reach No.~%s", d[d$Name == i, "ReachNo"]))
}

## ----include=FALSE-------------------------------------------------------
v <- "Assigned river reaches in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_reach, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
Pal <- function(...) {
  cols1 <- rainbow(..., s=0.5, v=1)
  cols2 <- rainbow(..., s=1, v=0.5)
  is.even <- seq_along(cols1) %% 2 == 0
  cols1[is.even] <- cols2[is.even]
  return(cols1)
}
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6, credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col="#FFFFFF9A", add=TRUE)
cols <- Pal(max(reach.numbers))[reach.numbers]
plot(rr, col=cols, lwd=0.5, add=TRUE)
text(getSpatialLinesMidPoints(rr), labels=reach.numbers,
     col="#333333", cex=0.6, pos=4, offset=0.4)
ids <- c("13135500", "13139510", "13140800", "13140900", "13150430")
points(sites[sites@data$SiteNo %in% ids, ], pch=24, cex=0.7,
       col="#333333B1", bg="#333333B1")
legend("topright", format(unique(reach.numbers)), lwd=1, col=unique(cols),
       pt.cex=1, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5,
       bg="#FFFFFFE7", title=expression(bold("Reach")))
invisible(dev.off())

## ----table_reach, echo=FALSE, results="asis"-----------------------------
d <- data.frame(ReachNo=seq_along(reach.names))

d$Name <- c("Big Wood, Nr Ketchum to Hailey",
            "Big Wood, Hailey to Stanton Crossing",
            "Willow Creek",
            "Silver Creek, above Sportsman Access",
            "Silver Creek, Sportsman Access to Nr Picabo")

# match(c(13135500, 13139510, 0, 0, 13150430), sites@data$SiteNo)
d$UpNo <- c("\\textbf{1}", "\\textbf{14}", NA, NA, "\\textbf{31}")

# match(c(13139510, 13140800, 13140900, 13150430, 13150500), sites@data$SiteNo)
d$DownNo <- c("\\textbf{14}", "\\textbf{20}", "\\textbf{21}", "\\textbf{31}", "32")

# match(c(13135520, 13137000, 13137500, 13138000), sites@data$SiteNo)
d$TribNo <- c("2, 9, 10, 12", NA, NA, NA, NA)

# lapply(d$Name, function(i) which(p.div@data$BigReach == i))
d$div.no <- c("1--27", "28--69", "70--79", "80--93", "94--98")

# lapply(d$Name, function(i) which(p.ret@data$BigReach == i))
d$ret.no <- c("1--4", "5", NA, "6--18", "19")

columns <- c("Reach \\\\ No.",
             "Name",
             "Upstream \\\\ Site No.",
             "Down- \\\\ stream \\\\ Site No.",
             "Tributary \\\\ Site No.",
             "Return \\\\ No.",
             "Diversion \\\\ No.")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Assigned river reaches of the Big Wood River, Willow Creek, Silver Creek, and spring-fed tributaries."
cap2 <- c("\\textbf{Reach No.}: identifier used to locate river reaches in \\hyperref[fig:map_reach]{figure~\\ref{fig:map_reach}}.",
          "\\textbf{Name}: local reach name used in this study.",
          "\\textbf{Site No.}: identifier used to locate streamflow  measurement sites in \\hyperref[fig:map_sites]{figure~\\ref{fig:map_sites}} and \\hyperref[table_sites]{table~\\ref{table_sites}}.",
            "Entry in \\textbf{bold} indicates a streamgage with continuous record.",
            "No upstream site number indicates that the reach is spring-fed.",
          "\\textbf{Return No.}: identifier used to locate return flows in \\hyperref[fig:map_ret]{figure~\\ref{fig:map_ret}} and \\hyperref[table_ret]{table~\\ref{table_ret}}.",
          "\\textbf{Diversion No.}: identifier used to locate stream diversions in \\hyperref[fig:map_div]{figure~\\ref{fig:map_div}} and \\hyperref[table_div]{table~\\ref{table_div}}.",
          "\\textbf{Abbreviations}: --, not present.")
tbl <- xtable::xtable(d, label="table_reach")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "l", "c", "c", "c", "c", "c")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="--",
      sanitize.text.function=identity)

## ----include=FALSE-------------------------------------------------------
v <- "Stream-aquifer flow exchange in the Big Wood River, near Ketchum to Hailey river reach."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_nket_hai, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d.rech <- reach.recharge
ndays <- GetDaysInMonth(tail(d.rech$YearMonth, 1))
d.rech <- data.frame(Date=as.Date(paste0(d.rech$YearMonth, "01"), "%Y%m%d"),
                     d.rech[, -1])
d.rech <- rbind(d.rech, d.rech[nrow(d.rech), , drop=FALSE])
d.rech$Date[nrow(d.rech)] <- d.rech$Date[nrow(d.rech)] + ndays
ylab <- paste("Stream-aquifer flow exchange, in", c("cubic meters per day", "cubic feet per second"))
xlim <- as.Date(c("1995-01-01", "2011-01-01"), tz="MST")
d <- d.rech[, c("Date", "nKet_Hai")]
col <- "#C80C0B"
PlotGraph(d, ylab=ylab, xlim=xlim, col=col, fill=paste0(col, "66"),
          conversion.factor=1 / cfs.to.m3.per.d,
          scientific=c(FALSE, TRUE, FALSE), center.date.labels=TRUE)
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "Stream-aquifer flow exchange in the Big Wood River, Hailey to Stanton Crossing river reach."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_hai_stc, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- d.rech[, c("Date", "Hai_StC")]
d[, 3] <- d[, 2]
d[!is.na(d[, 2]) & d[, 2] < 0, 2] <- 0
d[!is.na(d[, 3]) & d[, 3] > 0, 3] <- 0
col <- c("#67A9CF", "#C80C0B")
PlotGraph(d, ylab=ylab, xlim=xlim, col=col, fill=paste0(col, "66"),
          conversion.factor=1 / cfs.to.m3.per.d,
          scientific=c(FALSE, TRUE, FALSE), center.date.labels=TRUE)
legend("bottomright", c("Recharge", "Discharge"), col=col, lty=1,
       inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "Stream-aquifer flow exchange in the Willow Creek river reach."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_willowcr, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- d.rech[, c("Date", "WillowCr")]
col <- "#C80C0B"
PlotGraph(d, ylab=ylab, xlim=xlim, col=col, fill=paste0(col, "66"),
          conversion.factor=1 / cfs.to.m3.per.d,
          scientific=c(FALSE, TRUE, FALSE), center.date.labels=TRUE)
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "Stream-aquifer flow exchange in Silver Creek, above Sportsman Access river reach."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_silverabv, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- d.rech[, c("Date", "SilverAbv")]
col <- "#C80C0B"
PlotGraph(d, ylab=ylab, xlim=xlim, col=col, fill=paste0(col, "66"),
          conversion.factor=1 / cfs.to.m3.per.d,
          scientific=c(FALSE, TRUE, FALSE), center.date.labels=TRUE)
invisible(dev.off())

## ----echo=FALSE----------------------------------------------------------
subreaches <- river.reaches
is <- !subreaches@data$Reach %in% c("Willow Creek",
                                    "Big Wood, Stanton Crossing to Nr Bellevue",
                                    "Silver Creek, Sportsman Access to Nr Picabo")
subreaches <- subreaches[is, ]
subreaches$BigReachNo <- match(subreaches$BigReach, reach.names)

## ----include=FALSE-------------------------------------------------------
v <- "Assigned river subreaches in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_subreach, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
Pal <- function(...) {
  cols1 <- rainbow(..., s=0.5, v=1)
  cols2 <- rainbow(..., s=1, v=0.5)
  is.even <- seq_along(cols1) %% 2 == 0
  cols1[is.even] <- cols2[is.even]
  return(cols1)
}
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4],
        bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6, credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col="#FFFFFF9A", add=TRUE)
cols <- Pal(nlevels(subreaches@data$Reach))
plot(subreaches, col=cols, lwd=0.5, add=TRUE)
reach.numbers <- subreaches@data$ReachNo
pos <- c(4, 2, 4, 4, 4, 4, 4, 2, 2, 2,
         3, 3, 3, 3, 3, 3, 3, 3, 1)
text(getSpatialLinesMidPoints(subreaches), labels=reach.numbers,
     col="#333333", cex=0.6, pos=pos, offset=0.4)
pts <- sites[!sites@data$Type %in% c("Tributary", "Willow Creek") &
             !sites@data$Name %in% c("BIG WOOD RIVER NR BELLEVUE, ID",
                                     "SILVER CREEK NEAR HWY 20 NEAR PICABO, ID"), ]
points(pts, pch=24, cex=0.7, col="#333333B1", bg="#333333B1")
text(2478800, 1355000, labels="Big Wood River", cex=0.6, col="#333333", font=3, srt=75)
text(2485000, 1344500, labels="Silver\nCreek", cex=0.6, col="#333333", font=3, srt=0)
text(2483000, 1351800, labels="spring-fed\ntributaries", cex=0.6, col="#333333", font=3)
leg <- format(reach.numbers[order(reach.numbers)])
legend("topright", leg, lwd=1, col=cols[order(reach.numbers)], ncol=2,
       pt.cex=1, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5,
       bg="#FFFFFFE7", title=expression(bold("Subreach")))

invisible(dev.off())

## ----table_subreach, echo=FALSE, results="asis"--------------------------
d <- subreaches@data[order(subreaches@data$ReachNo), c("ReachNo", "Reach", "BigReachNo")]
d$BigReachNo[duplicated(d$BigReachNo)] <- ""
rownames(d) <- NULL

# match(c(13135500, 13135840, 13136000, 13138500, 13139510, 432929114174300, 432805114160400, 13140500, 432352114161500, 13140600,
#         NA, NA, NA, NA, NA, NA, NA, NA, NA), sites@data$SiteNo)

d$UpNo <- c("\\textbf{1}", 6, 8, 13, 14, 15, 16, 17, 18, NA, NA, NA, NA, NA, NA, NA, NA, NA, "23--30")

# match(c(13135840, 13136000, 13138500, 13139510, 432929114174300, 432805114160400, 13140500, 432352114161500, 13140600, 13140800,
#         13150010, 431947114133300, 13150140, 13150150, 13150300, 13150350, 13150360, 13150400, 13150430), sites@data$SiteNo)

d$DownNo <- c(6, 8, 13, 14, 15, 16, 17, 18, NA, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31)

# match(c(13135520, 434611114244600, 13135600, 13135700), sites@data$SiteNo)
# match(434404114215200, sites@data$SiteNo)
# match(c(13137000, 13137500, 433817114211800, 13138000), sites@data$SiteNo)

d$TribNo <- c("2--5", 7, "9--12", rep(NA, 16))

# with(p.ret@data, ret.no[which(Name %in% c("BYPASS CANAL ABV AND BLW DIVERSION NR BELLEVUE, ID"))])

# with(p.ret@data, ret.no[which(Name %in% c("A well into Buhler Drain"))])
# with(p.ret@data, ret.no[which(Name %in% c("Bickett Well 00P1"))])

# with(p.ret@data, ret.no[which(Name %in% c("Steve 0P", "Lucke Well 00P4 Flood", "Teeter Canyon 00-P5", "Prinz 0P6", "Meadow Well 18P1", "Rinker Well 18P", "Tick Tock 16P1", "Stalker 0P7"))])
# with(p.ret@data, ret.no[which(Name %in% c("Mill In 16P"))])

d$ret.no <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 20,
              21, 7, NA, NA, NA, NA, NA, NA, "8--10, 13--17")

# with(p.div@data, div.no[which(Name %in% c("Tom P2", "Don P3"))])

# with(p.div@data, div.no[which(Name %in% c("HIAWATHA CANAL AT POINT OF DIVERSON NR GIMLET, ID", "Aspen 27"))])
# with(p.div@data, div.no[which(Name %in% c("COVE CANAL AT POINT OF DIVERSON NR HAILEY, ID"))])

# with(p.div@data, div.no[which(Name %in% c("DISTRICT 45 CANAL AT POINT OF DIVERSON BELLVUE, ID", "Bannon 49", "GLENDALE CANAL AT POINT OF DIVERSON NR BELLVUE, ID", "BYPASS CANAL AT POINT OF DIVERSION NR BELLEVUE, ID"))])

# with(p.div@data, div.no[which(Name %in% c("Graff 62", "Uhrig 63", "Flood 64"))])
# with(p.div@data, div.no[which(Name %in% c("Salisbury 68", "Hice 71", "Martin 72", "Martin 72A", "Cloud 74", "Pugel 75"))])

# with(p.div@data, div.no[which(Name %in% c("Rogers 0P1"))])

# with(p.div@data, div.no[which(Name %in% c("Bill 9", "Loving 12B", "Stanfield 12P", "Stanfield 13", "Stanfield 13A"))])
# with(p.div@data, div.no[which(Name %in% c("Willis 1", "Gillihan 11", "Heath 10", "Patterson 15", "Albretheson 17", "Kilpatrick 18", "Iden 19", "Iden 19B"))])
# with(p.div@data, div.no[which(Name %in% c("Mantey 14P", "Iden 19P", "Man 19P1", "Tick Tock 19TT", "Div 21-23 and 27-29"))])

d$div.no <- c(NA, "8, 27", NA, "1, 99", 100, NA, "28, 101--103", NA, NA, "64, 67, 69",
              "89", NA, NA, NA, NA, NA, NA, "81, 87, 90--92", "80, 82--86, 88, 93")

d <- d[, c("BigReachNo", "ReachNo", "Reach", "UpNo", "DownNo", "TribNo", "ret.no", "div.no")]
columns <- c("Reach \\\\ No.",
             "Sub- \\\\ reach \\\\ No.",
             "Name",
             "Upstream \\\\ Site No.",
             "Down- \\\\ stream \\\\ Site No.",
             "Tributary \\\\ Site No.",
             "Return \\\\ No.",
             "Diversion \\\\ No.")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Assigned river subreaches of the Big Wood River, Silver Creek, and spring-fed tributaries."
cap2 <- c("\\textbf{Reach No.}: identifier used to locate river reaches in \\hyperref[fig:map_reach]{figure~\\ref{fig:map_reach}}.",
          "\\textbf{Subreach No.}: identifier used to locate river subreaches in \\hyperref[fig:map_subreach]{figure~\\ref{fig:map_subreach}}.",
          "\\textbf{Name}: local subreach name used in this study.",
          "\\textbf{Site No.}: identifier used to locate streamflow measurement sites in \\hyperref[fig:map_sites]{figure~\\ref{fig:map_sites}} and \\hyperref[table_sites]{table~\\ref{table_sites}}.",
            "No upstream site number indicates that the reach is spring-fed.",
          "\\textbf{Return No.}: identifier used to locate return flows in \\hyperref[fig:map_ret]{figure~\\ref{fig:map_ret}} and \\hyperref[table_ret]{table~\\ref{table_ret}}.",
          "\\textbf{Diversion No.}: identifier used to locate stream diversions in \\hyperref[fig:map_div]{figure~\\ref{fig:map_div}} and \\hyperref[table_div]{table~\\ref{table_div}}.",
          "\\textbf{Abbreviations}: --, not present")
tbl <- xtable::xtable(d, label="table_subreach")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "c", "p{2.0in}", "c", "c", "c", "c", "c")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="--",
      sanitize.text.function=identity)

## ----table_seepage, echo=FALSE, results="asis"---------------------------
d <- subreach.recharge[, c("ReachNo", "Reach", "Aug", "Oct", "Mar")]
d[, c("Aug_cfs", "Oct_cfs", "Mar_cfs")] <- d[, c("Aug", "Oct", "Mar")] * (1 / cfs.to.m3.per.d)
columns <- c("Subreach \\\\ No.", "Name",
             "Aug 2012 \\\\ (m\\textsuperscript{3}/d)",
             "Oct 2012 \\\\ (m\\textsuperscript{3}/d)",
             "Mar 2013 \\\\ (m\\textsuperscript{3}/d)",
             "Aug 2012 \\\\ (cfs)",
             "Oct 2012 \\\\ (cfs)",
             "Mar 2013 \\\\ (cfs)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Estimated stream-aquifer flow exchange in river subreaches for August 2012, October 2012, and March 2013; modified from \\citet{Bartolino2014}."
cap2 <- c("\\textbf{Subreach No.}: identifier used to locate river subreaches in \\hyperref[fig:map_subreach]{figure~\\ref{fig:map_subreach}}.",
          "\\textbf{Name}: local subreach name used in this study.",
          "\\textbf{Abbreviations}: m\\textsuperscript{3}/d, cubic meters per day; cfs, cubic-feet per second;  --, not available because of missing tributary inflows to the Big Wood River above Hailey")
tbl <- xtable::xtable(d, label="table_seepage")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "p{2.2in}", "r", "r", "r", "r", "r", "r")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE, NA.string="--",
      sanitize.colnames.function=function(x){x}, size="\\small", format.args=list(big.mark=","))

## ----table_pdiff, echo=FALSE, results="asis"-----------------------------
mo <- substr(reach.recharge$YearMonth, 5, 6)
yr <- unique(substr(reach.recharge$YearMonth, 1, 4))
reach.no <- sort(unique(subreach.recharge$BigReachNo))

x <- data.frame(BigReachNo=rep(reach.no, each=length(yr)))
x$Aug <- with(reach.recharge[mo == "08", ], c(nKet_Hai, Hai_StC, SilverAbv))
x$Oct <- with(reach.recharge[mo == "10", ], c(nKet_Hai, Hai_StC, SilverAbv))
x$Mar <- with(reach.recharge[mo == "03", ], c(nKet_Hai, Hai_StC, SilverAbv))
y <- aggregate(subreach.recharge[, c("Aug", "Oct", "Mar")], by=list(BigReachNo=subreach.recharge$BigReachNo), sum)
y <- y[match(x$BigReachNo, y$BigReachNo), ]

d <- (abs(x - y) / (abs(x) + abs(y) / 2) * 100)[, -1]
d$BigReachNo <- x$BigReachNo
d$BigReach <- as.character(subreach.recharge$BigReach[match(d$BigReachNo, subreach.recharge$BigReachNo)])
d$Year <- rep(yr, length(reach.no))
d <- d[, c("BigReachNo", "BigReach", "Year", "Aug", "Oct", "Mar")]

d <- d[as.integer(d$Year) >= 2000L, ]

is.dup <- duplicated(d$BigReachNo)
d$BigReachNo[is.dup] <- ""
d$BigReach[is.dup]   <- ""

columns <- c("Reach \\\\ No.", "Name", "Year",
             "August \\\\ (1)",
             "October \\\\ (1)",
             "March \\\\ (1)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Percent difference between stream-aquifer flow exchange values estimated in river reaches during August, October, and March of 2000 through 2010; and in river subreaches aggregated by reach during August 2012, October 2012, and March 2013."
cap2 <- c("\\textbf{Subreach No.}: identifier used to locate river subreaches in \\hyperref[fig:map_subreach]{figure~\\ref{fig:map_subreach}}.",
          "\\textbf{Name}: local reach name used in this study.",
          "\\textbf{Year}: the measurement year.",
          "\\textbf{Abbreviations}: --, not available")
tbl <- xtable::xtable(d, label="table_pdiff")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "c", "l", "c", "r", "r", "r")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE, NA.string="--",
      sanitize.colnames.function=function(x){x}, size="\\small", format.args=list(big.mark=","))

## ----table_ratio, echo=FALSE, results="asis"-----------------------------
d <- aggregate(subreach.recharge[, c("Aug", "Oct", "Mar")], by=list(BigReachNo=subreach.recharge$BigReachNo), sum)
d <- d[match(subreach.recharge$BigReachNo, d$BigReachNo), -1]
ratio <- subreach.recharge[, c("Aug", "Oct", "Mar")] / d
d <- data.frame(subreach.recharge[, c("ReachNo", "Reach")], ratio)
columns <- c("Subreach \\\\ No.", "Name",
             "Aug 2012 \\\\ (1)",
             "Oct 2012 \\\\ (1)",
             "Mar 2013 \\\\ (1)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Stream-aquifer flow-exchange ratio between river subreaches and their corresponding reach, for August 2012, October 2012, and March 2013."
cap2 <- c("\\textbf{Subreach No.}: identifier used to locate river subreaches in \\hyperref[fig:map_subreach]{figure~\\ref{fig:map_subreach}}.",
          "\\textbf{Name}: local subreach name used in this study.",
          "\\textbf{Abbreviations}: --, not available")
tbl <- xtable::xtable(d, label="table_ratio")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- c(0, 0, 0, 2, 2, 2)
xtable::align(tbl) <- c("l", "c", "l", "r", "r", "r")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE, NA.string="--",
      sanitize.colnames.function=function(x){x}, size="\\small", format.args=list(big.mark=","))

## ----include=FALSE-------------------------------------------------------
v <- "Location of the Silver Creek and Stanton Crossing groundwater outlet boundaries."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_drains, echo=FALSE, fig.width=fin.map.s.0[1], fig.height=fin.map.s.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
l <- rgeos::gIntersection(drains, as(alluvium.extent, "SpatialLinesDataFrame"), TRUE)
l <- SpatialLinesDataFrame(l, data=drains@data, match.ID=FALSE)
cols <- c("#F02311", "#FBB829")
PlotMap(crs(hill.shading), xlim=usr.map.s[1:2], ylim=usr.map.s[3:4], bg.image=hill.shading,
        bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit)
plot(alluvium.extent, border="#FFFFFFCC", col=NA, add=TRUE)
leg <- c("Stanton Crossing", "Silver Creek")
plot(l[match(l@data$Name, leg), ], col=cols, lwd=2, add=TRUE)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=3, offset=0.4)
legend("topleft", leg, lwd=2, col=cols, border=NA, inset=0.02, cex=0.7,
       box.lty=1, box.lwd=0.5, xpd=NA, bg="#FFFFFFE7", title=expression(bold("Outlet Boundaries")))
AddInsetMap(alluvium.extent, width=1, main.label=list("AQUIFER", adj=c(0, -9)),
            sub.label=list("Map area", adj=c(1.9, 0.5)), loc="topright")
invisible(dev.off())

## ----table_zones, echo=FALSE, results="asis"-----------------------------
d <- kriging.zones@data

FUN <- function(i) which(as.character(pilot.points@data$Zone) == i)
pnts.in.zone <- lapply(as.character(kriging.zones@data$Zone), FUN)
names(pnts.in.zone) <- kriging.zones@data$Zone
d$NumPoints <- sapply(pnts.in.zone, length)

# print(pnts.in.zone)
d$Points <- c("1, 3, 4, 6--8, 11, 14--17, 19, 23, 24, 26, 32, 35--38, 41--59",
              "30, 31", "33, 34", "39, 40", 2, 5, "12, 13", "9, 10", "20--22",
              18, 25, "27--29", "60--72, 74--76, 83, 84", "77--80", "73, 81, 82",
              "85--95, 97, 98, 105, 106", "99--102", "96, 103, 104")
d <- d[, c("Layer", "Zone", "Name", "NumPoints", "Points")]
d$Layer[duplicated(d$Layer)] <- ""
columns <- c("Model \\\\ layer", "Zone", "Name", "No. of \\\\ points", "Point No.")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Zones and pilot-points within the model domain."
cap2 <- c("\\textbf{Zone}: identifier used to locate a zone in \\hyperref[fig:map_pilot]{figure~\\ref{fig:map_pilot}}.",
          "\\textbf{Name}: local zone name used in this study.",
          "\\textbf{No. of points}: contained within a zone.",
          "\\textbf{Point No.}: identifier used to locate pilot points in \\hyperref[fig:map_pilot]{figure~\\ref{fig:map_pilot}}.")
tbl <- xtable::xtable(d, label="table_zones")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <-  c("l", "c", "c", "l", "r", "p{1.5in}")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="NA")

## ----echo=FALSE----------------------------------------------------------
FUN <- function(layer) {
  kz <- kriging.zones[kriging.zones@data$Layer == layer, ]
  zones <- kz@data$Zone
  r <- rasterize(kz, land.surface, "Zone")
  dc <- (layer - 1L) * 0.1
  Pal <- function(...) rainbow(..., s=1, v=0.5, start=0.1 + dc, end=1.0 - dc, alpha=0.6)
  PlotMap(r, xlim=usr.map[1:2], ylim=usr.map[3:4], pal=Pal,
          bg.image=hill.shading, dms.tick=TRUE, bg.image.alpha=0.6,
          rivers=list(x=streams.rivers), lakes=list(x=lakes), draw.key=FALSE, credit=credit)
  is.layer <- pilot.points@data$Layer == layer
  pp <- pilot.points[is.layer, ]
  points(pp, pch=21, cex=0.6, col="#333333", bg="#333333")
  text(pp, labels=seq_along(pilot.points)[is.layer], col="#333333",
       cex=0.6, pos=1, offset=0.3)

  text(kz, labels=zones, col="black", cex=0.7, pos=1, offset=0.5, font=2)
  labs <- as.character(sort(zones))
  cols <- Pal(length(labs))[as.integer(order(zones))]
  legend("topright", labs, pch=22, col=cols, pt.bg=cols,
         pt.cex=1, inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7",
         text.font=2, title=expression(bold("Zone")))
  legend("topright", "Pilot point", pch=21, col="#333333", pt.bg="#333333",
         pt.cex=0.6, inset=c(0.1, 0.02), cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")
}

## ----map_pilot_a, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN(1L)

## ----include=FALSE-------------------------------------------------------
v <- "Spatial distribution of the pilot points and zones in (\\textit{\\textbf{A}}) model layer~1, (\\textit{\\textbf{B}}) model layer~2, and (\\textit{\\textbf{C}}) model layer~3. \\label{fig:map_pilot}"
v <- c(paste("Maps showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_pilot_b, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN(2L)

## ----map_pilot_c, echo=FALSE, results="asis", fig.width=fin.map.0[1], fig.height=fin.map.0[2]----
FUN(3L)
invisible(dev.off())

## ----table_hk_sc, echo=FALSE, results="asis"-----------------------------
d <- zone.properties
d$zone <- c("A--M, P", "N, Q", "O", "R")
d$unit <- c("Alluvium (unconfined)", "Basalt", "Clay", "Alluvium (confined)")
d$hk <- ToScientific(d$hk, digits=1)
d$sc <- ToScientific(d$sc, digits=1)
d <- d[, c("zone", "unit", "hk", "vani", "sc")]
columns <- c("Zone",
             "Hydrogeologic unit",
             "Hydraulic \\\\ conductivity \\\\ (m/d)",
             "Vertical \\\\ anisotropy \\\\ (1)",
             "Storage \\\\ coefficient \\\\ (1)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Starting values of horizontal hydraulic conductivity, vertical anisotropy, and storage coefficient; values assigned to each zone in the model domain."
cap2 <- c("\\textbf{Hydraulic conductivity}: is the horizontal hydraulic conductivity.",
          "\\textbf{Vertical anisotropy}: is the ratio of horizontal to vertical hydraulic conductivity.",
          "\\textbf{Storage coefficient}: for saturated conditions it is the product of specific storage and the saturated thickness of the aquifer; for partially-saturated conditions it is virtually equal to the specific yield.",
          "\\textbf{Abbreviations}: m/d, meters per day")
tbl <- xtable::xtable(d, label="table_hk_sc")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- c(0, 0, 0, 1, 0, 1)
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      sanitize.text.function=identity, size="\\small")

## ----table_tribs, echo=FALSE, results="asis"-----------------------------
d <- tributaries@data
d <- data.frame(Name=d$Name, ID=seq_len(nrow(d)), d[, -1])
d <- d[, c("Name", "ID", "Flow", "PrecipFlow")]
d$s.up <- 0.2 * (d$PrecipFlow / d$Flow)
columns <- c("Name",
             "Trib \\\\ No.",
             "Flow \\\\ rate \\\\ $\\overline{Q}$ \\\\ (m\\textsuperscript{3}/d)",
             "Precip. \\\\ flow \\\\ $\\overline{Q}_{p}$ \\\\ (m\\textsuperscript{3}/d)",
             "Upper bound \\\\ on scalar \\\\ $s_{\\mathit{up}}$ \\\\ (1)")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Upper bound placed on scalar components of the mean tributary basin underflow."
cap2 <- c("\\textbf{Trib No.}: is an identifier used to locate the tributary model boundaries on the map in \\hyperref[fig:map_tribs]{figure~\\ref{fig:map_tribs}}.",
          "\\textbf{Flow rate}: is the empirically derived estimate of the mean tributary basin underflow.",
          "\\textbf{Precip. flow}: is the precipitation estimate of mean tributary basin underflow.",
          "\\textbf{Abbreviations}: m\\textsuperscript{3}/d, cubic meters per day")
tbl <- xtable::xtable(d, label="table_tribs")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- c(0, 0, 0, 0, 0, 1)
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      format.args=list(big.mark=","), sanitize.colnames.function=function(x){x},
      size="\\small")

## ----include=FALSE-------------------------------------------------------
v <- "Location of underflow boundaries in the major tributary canyons and the upper part of the Wood River Valley."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_tribs, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
PlotMap(crs(alluvium.thickness), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading, dms.tick=TRUE,
        bg.image.alpha=0.6, rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit)
cols <- c("#F02311", "#FFFFFFCC")
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
leg <- c("Underflow boundaries", "Aquifer extent")
legend("topright", leg, pch=c(15, 22), lwd=NA, col=cols, pt.bg="#33333366",
       pt.cex=c(0.7, 1.5), inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5,
       bg="#FFFFFFE7", seg.len=1.4)
invisible(dev.off())

## ----include=FALSE-------------------------------------------------------
v <- "Irrigation entities in the Wood River Valley, Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_eff, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
p <- irr.entities
p@data <- dplyr::left_join(p@data[, "EntityName", drop=FALSE], efficiency, by="EntityName")
PlotMap(crs(hill.shading), xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=hill.shading,
        bg.image.alpha=0.6, dms.tick=TRUE, rivers=list(x=streams.rivers), lakes=list(x=lakes), credit=credit)
cols <- c("#66C2A5B3", "#FC8D62B3")
plot(p, col=cols[as.integer(p@data$Comment)], border="#FFFFFFCC", lwd=0.5, add=TRUE)
plot(cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(cities, labels=cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
leg <- c("Irrigation entity", "Irrigation entity with possible natural sub-irrigation")
legend("topright", leg, fill=cols, border="#FFFFFFCC", inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, xpd=NA, bg="#FFFFFFE7")
invisible(dev.off())

## ----table_weights, echo=FALSE, results="asis"---------------------------
d <- data.frame(typ=c("Groundwater level", "", "", "", "",
                      "Stream-aquifer flow exchange", "", "", "", "",
                      "Stream-aquifer flow-exchange ratio",
                      "Groundwater discharge", "",
                      "Horizontal hydraulic conductivity", "",
                      "Vertical anisotropy",
                      "Storage coefficient",
                      "Riverbed conductance",
                      "Irrigation efficiency",
                      "Scalar component",
                      "Moving average duration",
                      "Amplitude-reduction factor"),
                grp=c("U.S. Geologic Survey monitoring network wells",
                      "Geolocated driller wells",
                      "Public Land Survey System-located driller wells",
                      "Sun Valley Water and Sewer production well",
                      "The Nature Conservancy monitoring network well",
                      "Big Wood River, Near Ketchum to Hailey reach",
                      "Big Wood River, Hailey to Stanton Crossing reach",
                      "Willow Creek reach",
                      "Silver Creek, above Sportsman Access reach",
                      "Silver Creek, Sportsman Access to near Picabo reach",
                      "Subreaches of the Big Wood River, Silver Creek, and spring-fed tributaries",
                      "Stanton Crossing outlet boundary",
                      "Silver Creek outlet boundary",
                      "Pilot points in each zone (\\hyperref[fig:map_pilot]{fig.~\\ref{fig:map_pilot}})",
                      "Pilot points in zones A, M, and P",
                      "Global value",
                      "Pilot points in each zone",
                      "Willow Creek reach and Silver Creek, Sportsman Access to Near Picabo reach; subreaches of the Big Wood River, Silver Creek, and spring-fed tributaries",
                      "Irrigated lands",
                      "Tributary boundaries",
                      "Global value for all tributary boundaries",
                      "Global value for all tributary boundaries"),
                wgt=c(1, 0.75, 0.2295, 0.5, 0.211527,
                      0.000317, 0.0000554, 0.000457, 0.000099, 0.000099,
                      10.73422, 0.01, 0.0001, NA, NA, NA, NA, 1, 10, NA, NA, NA),
                unt=c(rep("1/m", 5), rep("d/m\\textsuperscript{3}", 5),
                      "1", rep("d/m\\textsuperscript{3}", 2),
                      rep("d/m", 2), "1", "1", "d/m\\textsuperscript{2}", "1", "1", "1/d", "1"))
d$wgt <- ToScientific(d$wgt, digits=1)
d$wgt[is.na(d$wgt)] <- "ADJ"
columns <- c("Observation type", "Observation group", "Weight", "Units")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
cap1 <- "Observation weights assigned to field measurements and prior-information."
cap2 <- c("\\textbf{Abbreviations}: ADJ, automatically adjusted during the parameter estimation process; 1/m, inverse meters; d/m\\textsuperscript{3}, days per cubic meter; d/m\\textsuperscript{2}, days per square meter; 1/d, inverse days")
tbl <- xtable::xtable(d, label="table_weights")
xtable::caption(tbl) <- c(sprintf("%s [%s]", cap1, paste(cap2, collapse=" ")), cap1)
xtable::digits(tbl) <- 0
xtable::align(tbl) <- c("l", "l", "p{3.2in}", "l", "l")
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x}, size="\\small", NA.string="",
      sanitize.text.function=identity)

