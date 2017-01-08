## ----setup, include=FALSE------------------------------------------------
t0 <- Sys.time()
try(knitr::opts_chunk$set(tidy=FALSE, comment="#", fig.align="center"), silent=TRUE)
grDevices::pdf.options(useDingbats=FALSE)

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
usr.map <- c(2451504, 2497815, 1342484, 1402354)

# Map credit
credit <- paste("Base derived from U.S. Geological Survey National Elevation Dataset 10-meter digital elevation model.",
                "Idaho Transverse Mercator projection; North American Datum of 1983.", sep="\n")

CheckStatus <- function(s) {
  if (interactive()) {
    if (!isTRUE(all.equal(get(s), eval(parse(text=paste0("wrv::", s))), showwarning=FALSE, tolerance=1e-5))) {
      warning(paste0("dataset '", s, "' has changed"), call.=FALSE)
    }
  }
}

## ----warning=FALSE, message=FALSE, results="hide"------------------------
library("rgdal")   # bindings for the geospatial data abstraction library
library("raster")  # gridded spatial data toolkit

## ------------------------------------------------------------------------
rasterOptions(standardnames = FALSE)

## ----download_git, message=FALSE, results="hide"-------------------------
url <- "https://github.com/USGS-R/wrv.git"
path <-  file.path(tempdir(), basename(tools::file_path_sans_ext(url)))
git2r::clone(url, path, progress = FALSE)
dir.in <- file.path(path, "inst/extdata")
files <- list.files(dir.in, pattern = "*.zip$", full.names = TRUE, recursive = TRUE)
for (i in files) unzip(i, exdir = dirname(i))

## ----download_ned, message=FALSE-----------------------------------------
ftp <- "ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/NED/13/ArcGrid/n44w115.zip"
file <- file.path(tempdir(), basename(ftp))
download.file(ftp, file)
unzip(file, exdir = dir.in)

## ----create_data_dir-----------------------------------------------------
dir.create(dir.out <- "data", showWarnings = FALSE)

## ----unit_conversions----------------------------------------------------
in.to.m              <- 0.0254         # inches to meters
ft.to.m              <- 0.3048         # feet to meters
mm.to.m              <- 0.001          # millimeters to meters
mi2.to.m2            <- 2589990        # square miles to square meters
af.to.m3             <- 1233.48185532  # acre-feet to cubic meters
in.per.y.to.m.per.d  <- 6.95429e-05    # inches per year to meters per day
af.per.y.to.m3.per.d <- 3.377          # acre-feet per year to cubic meters per day
cfs.to.m3.per.d      <- 2446.57555     # cubic feet per second to cubic meters per day

## ----crs-----------------------------------------------------------------
crs <- CRS(paste("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000",
                 "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

## ----spatial-------------------------------------------------------------
ext <- extent(2453200, 2496100, 1344139, 1400639)  # xmin, xmax, ymin, ymax in IDTM
spatial.grid <- raster(crs = crs, ext = ext, resolution = 100)

## ----high_res_spatial----------------------------------------------------
high.res.spatial.grid <- disaggregate(spatial.grid, fact = 5L)

## ----temporal------------------------------------------------------------
tr.interval <- as.Date(c("1995-01-01", "2011-01-01"), tz = "MST")
tr.stress.periods <- seq(tr.interval[1] , tr.interval[2], "1 month")
yr.mo <- format(head(tr.stress.periods, -1), "%Y%m")
yr.mo.irr <- yr.mo[months(head(tr.stress.periods, -1), abbreviate = TRUE) %in%
                   c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")]

## ----canal_seep_1--------------------------------------------------------
file <- file.path(dir.in, "canal/canal.seep.csv")
canal.seep <- read.csv(file, strip.white = TRUE)
save(canal.seep, file = file.path(dir.out, "canal.seep.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("canal.seep")

## ----comb_sw_irr_1-------------------------------------------------------
file <- file.path(dir.in, "div/comb.sw.irr.csv")
comb.sw.irr <- read.csv(file, strip.white = TRUE)
comb.sw.irr$Pdate <- as.Date(comb.sw.irr$Pdate, format = "%m/%d/%Y")
comb.sw.irr$MaxDivRate <- comb.sw.irr$MaxDivRate * cfs.to.m3.per.d
save(comb.sw.irr, file = file.path(dir.out, "comb.sw.irr.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("comb.sw.irr")

## ----et_method_1---------------------------------------------------------
file <- file.path(dir.in, "et/et.method.csv")
et.method <- read.csv(file, strip.white = TRUE)
et.method$YearMonth <- as.character(et.method$YearMonth)
save(et.method, file = file.path(dir.out, "et.method.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("et.method")

## ----div_gw_1------------------------------------------------------------
file <- file.path(dir.in, "div/div.gw.csv")
div.gw <- read.csv(file, strip.white = TRUE)
div.gw$YearMonth <- as.factor(div.gw$YearMonth)
div.gw$GWDiv <- div.gw$GWDiv_af * af.to.m3
div.gw$GWDiv_af <- NULL
div.gw[is.na(div.gw$GWDiv), "GWDiv"] <- 0
save(div.gw, file = file.path(dir.out, "div.gw.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("div.gw")

## ----efficiency_1--------------------------------------------------------
file <- file.path(dir.in, "irr/efficiency.csv")
efficiency <- read.csv(file, strip.white = TRUE)
save(efficiency, file = file.path(dir.out, "efficiency.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("efficiency")

## ----irr_lands_year_1----------------------------------------------------
file <- file.path(dir.in, "irr/irr.lands.year.csv")
irr.lands.year <- read.csv(file, strip.white = TRUE, colClasses = "character")
save(irr.lands.year, file = file.path(dir.out, "irr.lands.year.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("irr.lands.year")

## ----swe_1---------------------------------------------------------------
file <- file.path(dir.in, "precip/swe.choco.csv")
swe.choco <- read.csv(file, strip.white = TRUE)
swe.choco$MonthDay <- format(as.Date(swe.choco$Date, "%m/%d/%Y"), "%m%d")
swe.choco$SWE <- swe.choco$SWE_in * in.to.m
file <- file.path(dir.in, "precip/swe.hailey.csv")
swe.hailey <- read.csv(file, strip.white = TRUE)
swe.hailey$MonthDay <- format(as.Date(swe.hailey$Date, "%m/%d/%Y"), "%m%d")
swe.hailey$SWE <- swe.hailey$SWE_in * in.to.m
file <- file.path(dir.in, "precip/swe.picabo.csv")
swe.picabo <- read.csv(file, strip.white = TRUE)
swe.picabo$MonthDay <- format(as.Date(swe.picabo$Date, "%m/%d/%Y"), "%m%d")
swe.picabo$SWE <- swe.picabo$SWE_in * in.to.m

## ----swe_2---------------------------------------------------------------
swe.choco  <- aggregate(swe.choco$SWE, list(swe.choco$MonthDay), mean)
swe.hailey <- aggregate(swe.hailey$SWE, list(swe.hailey$MonthDay), mean)
swe.picabo <- aggregate(swe.picabo$SWE, list(swe.picabo$MonthDay), mean)

## ----swe_3---------------------------------------------------------------
swe <- swe.choco[order(swe.choco[[1]]), ]
swe <- dplyr::left_join(swe, swe.hailey, by = "Group.1")
swe <- dplyr::left_join(swe, swe.picabo, by = "Group.1")
names(swe) <- c("MonthDay", "Choco", "Hailey", "Picabo")
save(swe, file = file.path(dir.out, "swe.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("swe")

## ----precipitation_1-----------------------------------------------------
file <- file.path(dir.in, "precip/precip.csv")
d <- read.csv(file, strip.white = TRUE)
d$Ketchum <- d$Ketchum_ft * ft.to.m
d$Hailey  <- d$Hailey_ft  * ft.to.m
d$Picabo  <- d$Picabo_ft  * ft.to.m
d$Ketchum_ft <- NULL
d$Hailey_ft  <- NULL
d$Picabo_ft  <- NULL

## ----precipitation_2-----------------------------------------------------
x <- apply(d[, c("Picabo", "Ketchum")], 1, mean)
y <- d$Hailey
LM <- lm(y ~ x)

## ----precipitation_3-----------------------------------------------------
is.na.hailey <- is.na(y)
d$Hailey[is.na.hailey] <- predict(LM, data.frame(x))[is.na.hailey]
precipitation <- d

## ----include=FALSE-------------------------------------------------------
v <- "Monthly precipitation depth at the Hailey HADS weather station in the Wood River Valley aquifer system, south-central Idaho."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_precip_hailey, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d <- precipitation
d <- data.frame(Date=as.Date(paste0(d$YearMonth, "01"), format="%Y%m%d"),
                Precip=d$Hailey)
d1 <- d[!is.na.hailey, ]
d2 <- d[ is.na.hailey, ]
cols <- "#327CCB"
ylab <- paste("Monthly precipitation, in", c("meters", "feet"))
ltys <- c(1, 2)
inlmisc::PlotGraph(d, xlim=tr.interval, ylab=ylab, col=cols, lty=0,
                   conversion.factor=1 / ft.to.m, center.date.labels=TRUE)
lines(d1, lty=ltys[1], col=cols, type="s")
lines(d2, lty=ltys[2], col=cols, type="s")
legend("topright", c("Measured", "Estimated"), col=cols, lty=ltys, inset=0.02,
       cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")

## ----precipitation_4-----------------------------------------------------
d <- precipitation
mo <- month.abb[as.integer(substr(precipitation$YearMonth, 5, 6))]
for (i in seq_along(mo)) {
  if (mo[i] == "Nov") {
    precipitation$Ketchum[i] <- d$Ketchum[i] * 0.25
    precipitation$Hailey[i]  <- d$Hailey[i]  * 0.75
    precipitation$Picabo[i]  <- d$Picabo[i]  * 0.75
  } else if (mo[i] %in% c("Dec", "Jan")) {
    precipitation$Ketchum[i] <- d$Ketchum[i] * 0.25
    precipitation$Hailey[i]  <- d$Hailey[i]  * 0.25
    precipitation$Picabo[i]  <- d$Picabo[i]  * 0.25
  } else if (mo[i] == "Feb") {
    precipitation$Ketchum[i] <- d$Ketchum[i] * 0.25
    precipitation$Hailey[i]  <- d$Hailey[i]  * 0.50
    precipitation$Picabo[i]  <- d$Picabo[i]  * 0.75
  } else if (mo[i] == "Mar") {
    precipitation$Ketchum[i] <- d$Ketchum[i] * 0.25
    precipitation$Hailey[i]  <- sum(d$Hailey[(i - 4L):i] * c(0.25, 0.75, 0.75, 0.50, 1))
    precipitation$Picabo[i]  <- sum(d$Picabo[(i - 4L):i] * c(0.25, 0.75, 0.75, 0.25, 1))
  } else if (mo[i] == "Apr") {
    precipitation$Ketchum[i] <- sum(d$Ketchum[(i - 5L):i] * c(rep(0.75, 5), 1))
  }
}
precipitation <- dplyr::left_join(d, precipitation, by = "YearMonth")
sites <- c("Ketchum", "Hailey", "Picabo")
names(precipitation) <- c("YearMonth", paste0(sites, ".raw"), sites)

## ----precipitation_5-----------------------------------------------------
date.time <- as.Date(paste0(precipitation[, "YearMonth"], "01"), "%Y%m%d")
precipitation <- precipitation[date.time >= tr.interval[1] & date.time < tr.interval[2], ]

## ----precipitation_6-----------------------------------------------------
d <- data.frame(YearMonth = as.factor(rep(as.character(precipitation$YearMonth), 3)),
                PrecipZone = rep(sites, each = nrow(precipitation)),
                Precip = NA, Precip.raw = NA)
d[d$PrecipZone == "Ketchum", 3:4] <- precipitation[, c("Ketchum", "Ketchum.raw")]
d[d$PrecipZone == "Hailey",  3:4] <- precipitation[, c("Hailey", "Hailey.raw")]
d[d$PrecipZone == "Picabo",  3:4] <- precipitation[, c("Picabo", "Picabo.raw")]

## ----precipitation_7-----------------------------------------------------
precipitation <- d
save(precipitation, file = file.path(dir.out, "precipitation.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("precipitation")

## ----priority_cuts_1-----------------------------------------------------
file <- file.path(dir.in, "div/priority.cuts.csv")
priority.cuts <- read.csv(file, strip.white = TRUE)
priority.cuts$YearMonth <- as.factor(priority.cuts$YearMonth)
priority.cuts$Pdate_BWR <- as.Date(priority.cuts$Pdate_BWR, format = "%m/%d/%Y")
priority.cuts$Pdate_SC <- as.Date(priority.cuts$Pdate_SC, format = "%m/%d/%Y")
save(priority.cuts, file = file.path(dir.out, "priority.cuts.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("priority.cuts")

## ----div_sw_1------------------------------------------------------------
file <- file.path(dir.in, "div/div.sw.csv")
div.sw <- read.csv(file, strip.white = TRUE)
div.sw$YearMonth <- as.factor(div.sw$YearMonth)
div.sw$SWDiv <- div.sw$SWDiv_af * af.to.m3
div.sw$SWDiv_af <- NULL
div.sw[is.na(div.sw$SWDiv), "SWDiv"] <- 0
save(div.sw, file = file.path(dir.out, "div.sw.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("div.sw")

## ----div_ww_1------------------------------------------------------------
file <- file.path(dir.in, "div/div.ww.csv")
div.ww <- read.csv(file, strip.white = TRUE)
div.ww$YearMonth <- as.factor(div.ww$YearMonth)
div.ww$WWDiv <- div.ww$WWTP_af * af.to.m3
div.ww$WWTP_af <- NULL
div.ww[is.na(div.ww$WWDiv), "WWDiv"] <- 0
save(div.ww, file = file.path(dir.out, "div.ww.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("div.ww")

## ----gage_disch_1--------------------------------------------------------
FUN <- function(i) {
  file <- file.path(dir.in, "gage", i)
  d <- read.csv(file, colClasses = "character", strip.white = TRUE)
  d$Date <- as.Date(d$Date, format = "%Y-%m-%d")
  d$Disch <- suppressWarnings(as.numeric(d$Disch_cfs)) * cfs.to.m3.per.d
  return(d[substr(d$Code, 1, 1) == "A" & !is.na(d$Disch), c("Date", "Disch")])
}
gage.13135500.disch <- FUN("gage.13135500.disch.csv")
gage.13139510.disch <- FUN("gage.13139510.disch.csv")
gage.13140800.disch <- FUN("gage.13140800.disch.csv")

## ----gage_disch_2--------------------------------------------------------
dlim <- range(c(gage.13135500.disch$Date, gage.13139510.disch$Date,
                gage.13140800.disch$Date))
d <- data.frame(Date=seq(dlim[1], dlim[2], by = "day"))
d <- dplyr::left_join(d, gage.13135500.disch, by = "Date")
d <- dplyr::left_join(d, gage.13139510.disch, by = "Date")
d <- dplyr::left_join(d, gage.13140800.disch, by = "Date")
colnames(d) <- c("Date", "13135500", "13139510", "13140800")
gage.disch <- d
save(gage.disch, file = file.path(dir.out, "gage.disch.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("gage.disch")

## ----gage_height_1-------------------------------------------------------
FUN <- function(i) {
  file <- file.path(dir.in, "gage", i)
  d <- read.csv(file, colClasses = "character", strip.white = TRUE)
  d$DateTime <- strptime(d$DateTime, "%Y-%m-%d %H:%M", tz = "MST")
  d$Date <- as.Date(d$DateTime)
  d$Height <- suppressWarnings(as.numeric(d$Height_ft)) * ft.to.m
  d <- d[substr(d$Code, 1, 1) %in% c("W", "R", "A") & !is.na(d$Height), ]
  d <- aggregate(d$Height, list(d$Date), mean, na.rm = TRUE)
  names(d) <- c("Date", "Height")
  return(d)
}
gage.13135500.height <- FUN("gage.13135500.height.csv")
gage.13139510.height <- FUN("gage.13139510.height.csv")
gage.13140800.height <- FUN("gage.13140800.height.csv")

## ----gage_height_2-------------------------------------------------------
dlim <- range(c(gage.13135500.height$Date, gage.13139510.height$Date,
                gage.13140800.height$Date))
d <- data.frame(Date=seq(dlim[1], dlim[2], by = "day"))
d <- dplyr::left_join(d, gage.13135500.height, by = "Date")
d <- dplyr::left_join(d, gage.13139510.height, by = "Date")
d <- dplyr::left_join(d, gage.13140800.height, by = "Date")
colnames(d) <- c("Date", "13135500", "13139510", "13140800")

## ----gage_height_3-------------------------------------------------------
d[d < 0] <- NA

## ----gage_height_4-------------------------------------------------------
x <- d[["13139510"]]
y <- d[["13135500"]]
LM <- lm(y ~ x)

## ----gage_height_5-------------------------------------------------------
is.na.13135500 <- is.na(y)
d[["13135500"]][is.na.13135500] <- predict(LM, data.frame(x))[is.na.13135500]

## ----gage_height_6-------------------------------------------------------
jday <- as.integer(julian(as.Date(paste0("1900-", format(d$Date, "%m-%d"))),
                          origin = as.Date("1899-12-31")))
m <- cbind(jday, height = d[["13140800"]])
m <- m[rowSums(is.na(m)) == 0, ]
m0 <- m[m[, "jday"] > 300, ]
m1 <- m[m[, "jday"] < 66, ]
m0[, "jday"] <- m0[, "jday"] - 365
m1[, "jday"] <- m1[, "jday"] + 365
LPM <- loess(height ~ jday, data.frame(rbind(m0, m, m1)), span = 1 / 35)
ave.heights <- predict(LPM, newdata = 1:365)[jday]
is.na.13140800 <- is.na(d[["13140800"]])
d[["13140800"]][is.na.13140800] <- ave.heights[is.na.13140800]

## ----gage_height_7-------------------------------------------------------
gage.height <- d
save(gage.height, file = file.path(dir.out, "gage.height.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("gage.height")

## ----include=FALSE-------------------------------------------------------
v <- "Gage heights recorded at streamgages along the Big Wood River."
v <- c(paste("Graph showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----graph_gage_height, echo=FALSE, fig.width=fin.graph.short[1], fig.height=fin.graph.short[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
d1 <- gage.height
d2 <- gage.height
d1[ is.na.13135500, "13135500"] <- NA
d2[!is.na.13135500, "13135500"] <- NA
d1[ is.na.13140800, "13140800"] <- NA
d2[!is.na.13140800, "13140800"] <- NA
x <- merge(d1, d2, by = "Date")[d$Date >= tr.interval[1] & d$Date < tr.interval[2], ]
cols <- rep(c("#1B9E77", "#D95F02", "#7570B3"), 2)
ltys <- c(rep(1, 3), rep(3, 3))
ylab <- paste("Gage height, in", c("meters", "feet"))
inlmisc::PlotGraph(x, ylab=ylab, col=cols, lty=ltys, conversion.factor=1 / ft.to.m,
                   center.date.labels=TRUE)
leg <- c(sprintf("%s measured",  names(d)[-1]),
         sprintf("%s estimated", names(d)[-1]))
legend("topright", leg, col=cols, lty=ltys, inset=0.02, cex=0.7, box.lty=1,
       box.lwd=0.5, bg="#FFFFFFE7")

## ----pod_gw_1------------------------------------------------------------
file <- file.path(dir.in, "div/pod.gw.csv")
d <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
d$Pdate <- as.Date(d$PriorityDa, format = "%m/%d/%Y")
d$IrrRate <- d$IRRcfs * cfs.to.m3.per.d
columns <- c("WMISNumber", "WaterRight", "EntityName", "EntitySrce", "Pdate", "IrrRate")
pod.gw <- d[, columns]
save(pod.gw, file = file.path(dir.out, "pod.gw.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("pod.gw")

## ----misc_seepage_1------------------------------------------------------
file <- file.path(dir.in, "misc.seepage.csv")
d <- read.csv(file, strip.white = TRUE)
d$Rech <- d$Rech_af * af.to.m3
FUN <- function(i) {
  x <- d[d$YearMonth == i, c("RechSite", "Rech")]
  colnames(x) <- c("RechSite", i)
  return(x)
}
l <- lapply(yr.mo, FUN)
misc.seepage <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "RechSite"), l)
save(misc.seepage, file = file.path(dir.out, "misc.seepage.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("misc.seepage")

## ----zone_properties_1---------------------------------------------------
file <- file.path(dir.in, "zone.properties.csv")
d <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
d$hk <- d$hk_ft.per.d * ft.to.m
d$hk_ft.per.d <- NULL
d$ss <- d$ss_per.ft / ft.to.m
d$ss_per.ft <- NULL
zone.properties <- d
save(zone.properties, file = file.path(dir.out, "zone.properties.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("zone.properties")

## ----drybed_1------------------------------------------------------------
file <- file.path(dir.in, "perennial.reaches.csv")
perennial.reaches <- read.csv(file, colClasses = "character", strip.white = TRUE)[, 1]
drybed <- as.data.frame(matrix(NA, nrow = length(perennial.reaches), ncol = length(yr.mo)))
colnames(drybed) <- yr.mo
rownames(drybed) <- perennial.reaches
drybed[perennial.reaches, ] <- TRUE

## ----drybed_2------------------------------------------------------------
file <- file.path(dir.in, "canal/bypass.canal.op.csv")
d <- read.csv(file, colClasses = "character", strip.white = TRUE)
date1 <- as.Date(d$StartDate, tz = "MST")
date2 <- as.Date(d$EndDate, tz = "MST")
FUN <- function(i) {
  d <- as.integer(format(i, format = "%d"))
  m <- format(i, format = "%m")
  while (format(i, format = "%m") == m) i <- i + 1L
  return(d / as.integer(format(i - 1L, format = "%d")))
}
frac1 <- vapply(date1, FUN, 0)
frac2 <- vapply(date2, FUN, 0)
date1[frac1 > 0.5] <- date1[frac1 > 0.5] + 16L
date2[frac2 < 0.5] <- date2[frac2 < 0.5] - 16L
date1 <- as.Date(paste0(format(date1, format = "%Y-%m"), "-01"))
date2 <- as.Date(paste0(format(date2, format = "%Y-%m"), "-01"))
FUN <- function(i) format(seq(date1[i], date2[i], by = "month"), format = "%Y%m")
is.drybed <- yr.mo %in% unlist(lapply(seq_along(date1), FUN))
episodic.reaches <- c("Big Wood, Glendale to Sluder",
                      "Big Wood, Sluder to Wood River Ranch",
                      "Big Wood, Wood River Ranch to Stanton Crossing")
for (i in episodic.reaches) drybed[i, ] <- is.drybed

## ----drybed_3------------------------------------------------------------
drybed["Big Wood, Wood River Ranch to Stanton Crossing",
       substr(colnames(drybed), 5, 6) == "10"] <- FALSE

## ----drybed_4------------------------------------------------------------
drybed <- data.frame(Reach = rownames(drybed), drybed, check.names = FALSE,
                     row.names = NULL, stringsAsFactors = FALSE)
save(drybed, file = file.path(dir.out, "drybed.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("drybed")

## ----obs_wells_head_1----------------------------------------------------
file <- file.path(dir.in, "opt/obs.wells.head.csv")
d <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
d$DateTime <- as.POSIXct(d$DateTime, tz = "MST", format = "%Y-%m-%d %H:%M:%S")
d$Head <- as.numeric(d$Head_m)
d$Head_m <- NULL
obs.wells.head <- d
save(obs.wells.head, file = file.path(dir.out, "obs.wells.head.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("obs.wells.head")

## ----reach_recharge_1----------------------------------------------------
file <- file.path(dir.in, "opt/reach.recharge.csv")
d <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
d$YearMonth <- as.character(d$YearMonth)
d$nKet_Hai <- d$nKet_Hai_cfs * cfs.to.m3.per.d
d$nKet_Hai_cfs <- NULL
d$Hai_StC <- d$Hai_StC_cfs * cfs.to.m3.per.d
d$Hai_StC_cfs <- NULL
d$WillowCr <- d$WillowCr_cfs * cfs.to.m3.per.d
d$WillowCr_cfs <- NULL
d$SilverAbv <- d$SilverAbv_cfs * cfs.to.m3.per.d
d$SilverAbv_cfs <- NULL
d$SilverBlw <- d$SilverBlw_cfs * cfs.to.m3.per.d
d$SilverBlw_cfs <- NULL
reach.recharge <- d
save(reach.recharge, file = file.path(dir.out, "reach.recharge.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("reach.recharge")

## ----subreach_recharge_1-------------------------------------------------
file <- file.path(dir.in, "opt/subreach.recharge.csv")
d <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
d[, c("Aug", "Oct", "Mar")] <- d[, c("Aug_cfs", "Oct_cfs", "Mar_cfs")] * cfs.to.m3.per.d
d$Aug_cfs <- NULL
d$Oct_cfs <- NULL
d$Mar_cfs <- NULL
subreach.recharge <- d
save(subreach.recharge, file = file.path(dir.out, "subreach.recharge.rda"),
     compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("subreach.recharge")

## ----sensitivity_1-------------------------------------------------------
file <- file.path(dir.in, "opt/sensitivity.csv")
sensitivity <- read.csv(file, strip.white = TRUE)
sensitivity$parameter.name <- as.character(sensitivity$parameter.name)
rel <- with(sensitivity, comp.sens * abs(value))  # Realtive Composite Sensitivity
is.log <- sensitivity$parameter.desc %in% c("Horizontal hydraulic conductivity",
                                            "Storage coefficient",
                                            "Riverbed conductance",
                                            "Drain conductance")
rel[is.log] <- with(sensitivity, comp.sens * abs(log10(value)))[is.log]
sensitivity$rel.comp.sens <- rel
save(sensitivity, file = file.path(dir.out, "sensitivity.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("sensitivity")

## ----cities_1------------------------------------------------------------
path <- file.path(dir.in, "decorative")
cities <- readOGR(path, "cities", verbose = FALSE, integer64 = "allow.loss")
cities <- spTransform(cities, crs)
cities <- cities[cities@data$FEATURE_NA != "Elkhorn Village", ]
save(cities, file = file.path(dir.out, "cities.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("cities")

## ----map_labels_1--------------------------------------------------------
file <- file.path(dir.in, "decorative/map.labels.csv")
map.labels <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
map.labels$label <- sub("\\\\n", "\\\n", map.labels$label)
coordinates(map.labels) <- 1:2
colnames(map.labels@coords) <- c("x", "y")
proj4string(map.labels) <- CRS("+init=epsg:4326")
map.labels <- spTransform(map.labels, crs)
save(map.labels, file = file.path(dir.out, "map.labels.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("map.labels")

## ----misc_locations_1----------------------------------------------------
file <- file.path(dir.in, "decorative/misc.locations.csv")
misc.locations <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
misc.locations$label <- sub("\\\\n", "\\\n", misc.locations$label)
coordinates(misc.locations) <- 1:2
colnames(misc.locations@coords) <- c("x", "y")
proj4string(misc.locations) <- CRS("+init=epsg:4326")
misc.locations <- spTransform(misc.locations, crs)
save(misc.locations, file = file.path(dir.out, "misc.locations.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("misc.locations")

## ----weather_stations_1--------------------------------------------------
file <- file.path(dir.in, "precip/weather.stations.csv")
weather.stations <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
weather.stations$elevation <- weather.stations$elevation_ft * ft.to.m
weather.stations$elevation_ft <- NULL
weather.stations$url <- NULL
coordinates(weather.stations) <- 1:2
colnames(weather.stations@coords) <- c("x", "y")
proj4string(weather.stations) <- CRS("+init=epsg:4326")
weather.stations <- spTransform(weather.stations, crs)
save(weather.stations, file = file.path(dir.out, "weather.stations.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("weather.stations")

## ----pod_wells_1---------------------------------------------------------
path <- file.path(dir.in, "div")
pod.wells <- readOGR(path, "pod.wells", verbose = FALSE, integer64 = "allow.loss")
pod.wells <- spTransform(pod.wells, crs)
d <- pod.wells@data
columns <- c("TopOpen1", "BotOpen1", "TopOpen2", "BotOpen2")
d[, columns] <- d[, columns] * ft.to.m
d[d$TopOpen1 == 0 | d$BotOpen1 == 0, c("TopOpen1", "BotOpen1")] <- NA
d[d$TopOpen2 == 0 | d$BotOpen2 == 0, c("TopOpen2", "BotOpen2")] <- NA

## ----pod_wells_2, results="hide"-----------------------------------------
is.pred <- is.na(d$TopOpen1)
dists <- as.matrix(dist(coordinates(pod.wells)))
dists <- dists[!is.pred & d$WellUse %in% "Irrigation", ]
nearest.well <- as.integer(apply(dists, 2, function(i) names(which.min(i))))
d$TopOpen1[is.pred] <- d$TopOpen1[nearest.well[is.pred]]
d$BotOpen1[is.pred] <- d$BotOpen1[nearest.well[is.pred]]
columns <- c("WMISNumber", "WellUse", "TopOpen1", "BotOpen1", "TopOpen2", "BotOpen2")
pod.wells@data <- d[, columns]
save(pod.wells, file = file.path(dir.out, "pod.wells.rda"), compress = "xz")

## ----include=FALSE-------------------------------------------------------
v <- "Location of pumping wells and their completion status in the Wood River Valley aquifer system, south-central Idaho."
v <- c(paste("Map showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----map_completions, echo=FALSE, fig.width=fin.map.0[1], fig.height=fin.map.0[2], fig.scap=sprintf("{%s}", v[1]), fig.cap=sprintf("{%s}", v[2])----
inlmisc::PlotMap(crs, xlim=usr.map[1:2], ylim=usr.map[3:4], bg.image=wrv::hill.shading,
                 dms.tick=TRUE, bg.image.alpha=0.6, rivers=list(x=wrv::streams.rivers),
                 lakes=list(x=wrv::lakes), credit=credit, scale.loc="bottomleft")
plot(wrv::alluvium.extent, border="#FFFFFFCC", add=TRUE)
cols <- c("#9061C2D9", "#FBB829D9")
pchs <- c(24, 21)
pt.cexs <- c(0.5, 0.8)
points(pod.wells[ is.pred, ], pch=pchs[1], cex=pt.cexs[1], lwd=0.5, col=NA, bg=cols[1])
points(pod.wells[!is.pred, ], pch=pchs[2], cex=pt.cexs[2], lwd=0.5, col=NA, bg=cols[2])
plot(wrv::cities, pch=15, cex=0.8, col="#333333", add=TRUE)
text(wrv::cities, labels=wrv::cities@data$FEATURE_NA, col="#333333", cex=0.5, pos=1, offset=0.4)
leg <- paste(c("Estimated", "Measured"), "well completion")
legend("topright", leg, col=NA, pt.bg=cols, pch=pchs, pt.cex=pt.cexs, pt.lwd=0.5,
       inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFE7")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("pod.wells")

## ----streamgages_1-------------------------------------------------------
path <- file.path(dir.in, "gage")
streamgages <- readOGR(path, "streamgages", verbose = FALSE)
streamgages <- spTransform(streamgages, crs)
save(streamgages, file = file.path(dir.out, "streamgages.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("streamgages")

## ----obs_wells_1---------------------------------------------------------
path <- file.path(dir.in, "opt")
obs.wells <- readOGR(path, "obs.wells", verbose = FALSE, stringsAsFactors = FALSE,
                     integer64 = "allow.loss")
obs.wells <- spTransform(obs.wells, crs)
d <- obs.wells@data
d$COMPLETION <- as.Date(d$COMPLETION, tz = "MST", format = "%Y-%m-%d")
d$ALTMETHOD  <- as.factor(d$ALTMETHOD)
d$XYMETHOD   <- as.factor(d$XYMETHOD)
d$COUNTYNAME <- as.factor(d$COUNTYNAME)
d$desc       <- as.factor(d$desc)
d$TopOpen1 <- as.numeric(d$OPENINGMIN) * ft.to.m
d$BotOpen1 <- as.numeric(d$OPENINGMAX) * ft.to.m
d$TopOpen2 <- NA
d$BotOpen2 <- NA

## ----obs_wells_2---------------------------------------------------------
is.pred <- is.na(d$TopOpen1)
dists <- as.matrix(dist(coordinates(obs.wells)))[!is.pred, ]
nearest.well <- as.integer(apply(dists, 2, function(i) names(which.min(i))))
d$TopOpen1[is.pred] <- d$TopOpen1[nearest.well[is.pred]]
d$BotOpen1[is.pred] <- d$BotOpen1[nearest.well[is.pred]]
obs.wells@data <- d

## ----obs_wells_3---------------------------------------------------------
save(obs.wells, file = file.path(dir.out, "obs.wells.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("obs.wells")

## ----seepage_study_1-----------------------------------------------------
path <- file.path(dir.in, "opt")
seepage.study <- readOGR(path, "seepage.study", verbose = FALSE, integer64 = "allow.loss")
seepage.study <- spTransform(seepage.study, crs)
d <- seepage.study@data
d$Aug <- d$Aug_cfs * cfs.to.m3.per.d
d$Oct <- d$Oct_cfs * cfs.to.m3.per.d
d$Mar <- d$Mar_cfs * cfs.to.m3.per.d
d$Aug_cfs <- NULL
d$Oct_cfs <- NULL
d$Mar_cfs <- NULL
seepage.study@data <- d
save(seepage.study, file = file.path(dir.out, "seepage.study.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("seepage.study")

## ----div_ret_exch_1------------------------------------------------------
path <- file.path(dir.in, "opt")
div.ret.exch <- readOGR(path, "div.ret.exch", verbose = FALSE)
div.ret.exch <- spTransform(div.ret.exch, crs)
save(div.ret.exch, file = file.path(dir.out, "div.ret.exch.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("div.ret.exch")

## ----pilot_points_1------------------------------------------------------
path <- file.path(dir.in, "opt")
pilot.points <- readOGR(path, "pilot.points", verbose = FALSE, integer64 = "allow.loss")
pilot.points <- spTransform(pilot.points, crs)
save(pilot.points, file = file.path(dir.out, "pilot.points.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("pilot.points")

## ----canals_1------------------------------------------------------------
path <- file.path(dir.in, "canal")
canals <- readOGR(path, "canals", verbose = FALSE)
canals <- spTransform(canals, crs)
canals@data$Name <- as.character(canals@data$NAME)
canals@data <- canals@data[, c("EntityName", "Name")]
save(canals, file = file.path(dir.out, "canals.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("canals")

## ----river_reaches_1-----------------------------------------------------
river.reaches <- readOGR(dir.in, "river.reaches", verbose = FALSE, integer64 = "allow.loss")
river.reaches <- spTransform(river.reaches, crs)
d <- river.reaches@data
d$RchAvg <- d$RchAvg_cfs * cfs.to.m3.per.d
d$BigRAv <- d$BigRAv_cfs * cfs.to.m3.per.d
d$Depth  <- d$Depth_ft  * ft.to.m
d$BedThk <- d$BedThk_ft * ft.to.m
columns <- c("Reach", "BigReach", "DrainRiver", "RchAvg", "BigRAv", "ReachNo",
             "Depth", "BedThk")
river.reaches@data <- d[, columns]
save(river.reaches, file = file.path(dir.out, "river.reaches.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("river.reaches")

## ----streams_rivers_1----------------------------------------------------
path <- file.path(dir.in, "decorative")
streams.rivers <- readOGR(path, "rivers", verbose = FALSE, integer64 = "allow.loss")
streams.rivers <- spTransform(streams.rivers, crs)
save(streams.rivers, file = file.path(dir.out, "streams.rivers.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("streams.rivers")

## ----tributary_streams_1-------------------------------------------------
path <- file.path(dir.in, "decorative")
tributary.streams <- readOGR(path, "tributary.streams", verbose = FALSE,
                             integer64 = "allow.loss")
tributary.streams <- spTransform(tributary.streams, crs)
save(tributary.streams, file = file.path(dir.out, "tributary.streams.rda"),
     compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("tributary.streams")

## ----bypass_canal_1------------------------------------------------------
path <- file.path(dir.in, "canal")
bypass.canal <- readOGR(path, "bypass.canal", verbose = FALSE)
bypass.canal <- spTransform(bypass.canal, crs)
bypass.canal <- as(bypass.canal, "SpatialLines")
save(bypass.canal, file = file.path(dir.out, "bypass.canal.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("bypass.canal")

## ----wl_200610_1---------------------------------------------------------
wl.200610 <- readOGR(dir.in, "wl.200610", verbose = FALSE)
wl.200610 <- spTransform(wl.200610, crs)
wl.200610@data$CONTOUR <- wl.200610@data$CONTOUR * ft.to.m
save(wl.200610, file = file.path(dir.out, "wl.200610.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("wl.200610")

## ----major_roads_1-------------------------------------------------------
path <- file.path(dir.in, "decorative")
major.roads <- readOGR(path, "major.roads", verbose = FALSE)
major.roads <- spTransform(major.roads, crs)
save(major.roads, file = file.path(dir.out, "major.roads.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("major.roads")

## ----alluvium_extent_1---------------------------------------------------
path <- file.path(dir.in, "extent")
alluvium.extent <- readOGR(path, "alluvium.extent", verbose = FALSE,
                           integer64 = "allow.loss")
alluvium.extent <- spTransform(alluvium.extent, crs)
save(alluvium.extent, file = file.path(dir.out, "alluvium.extent.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("alluvium.extent")

## ----clay_extent_1-------------------------------------------------------
path <- file.path(dir.in, "extent")
clay.extent <- readOGR(path, "clay.extent", verbose = FALSE, integer64 = "allow.loss")
clay.extent <- spTransform(clay.extent, crs)
save(clay.extent, file = file.path(dir.out, "clay.extent.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("clay.extent")

## ----basalt_extent_1-----------------------------------------------------
path <- file.path(dir.in, "extent")
basalt.extent <- readOGR(path, "basalt.extent", verbose = FALSE)
basalt.extent <- spTransform(basalt.extent, crs)
save(basalt.extent, file = file.path(dir.out, "basalt.extent.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("basalt.extent")

## ----drains_1------------------------------------------------------------
file <- file.path(dir.in, "drains.kml")
p <- suppressWarnings(readOGR(file, basename(file), verbose = FALSE))
p@data$Description <- NULL
file <- file.path(dir.in, "drains.csv")
d <- read.csv(file, strip.white = TRUE)
p@data <- dplyr::left_join(p@data, d, by = "Name")
p <- spTransform(p, crs)
drains <- p
save(drains, file = file.path(dir.out, "drains.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("drains")

## ----tributaries_1-------------------------------------------------------
file <- file.path(dir.in, "tributaries.kml")
p <- suppressWarnings(readOGR(file, basename(file), verbose = FALSE))
p <- spTransform(p, crs)
p@data$Description <- NULL

## ----tributaries_2-------------------------------------------------------
file <- file.path(dir.in, "tributaries.csv")
d <- read.csv(file, strip.white = TRUE)
names(d) <- sub("_m$", "", names(d))
d$K <- d$K_ft.per.d * ft.to.m
d$K_ft.per.d <- NULL
d$SatArea <- (pi * d$TribWidth * d$BdrkDepth) / 4  # area of lower-half of ellipse
d$DarcyFlow <- d$K * d$SatArea * d$LandGrad

## ----tributaries_3-------------------------------------------------------
d$BasinArea <- d$BasinArea_mi2 * mi2.to.m2
d$BasinArea_mi2 <- NULL
natural.basin.area.break <- 2.59e+7  # 10 square-miles
is.small <- d$BasinArea < natural.basin.area.break
d$BasinAreaType <- as.factor(c("big", "small")[is.small + 1L])

## ----tributaries_4-------------------------------------------------------
d$PrecipRate <- d$PrecipRate_in.per.yr * in.per.y.to.m.per.d
d$PrecipRate_in.per.yr <- NULL
d$PrecipFlow <- d$BasinArea * d$PrecipRate

## ----tributaries_5-------------------------------------------------------
d$FlowRatio <- d$DarcyFlow / d$PrecipFlow

## ----tributaries_6-------------------------------------------------------
d$Flow <- d$DarcyFlow
d$Flow[is.small] <- d$PrecipFlow[is.small] * mean(d$FlowRatio[!is.small])

## ----tributaries_7-------------------------------------------------------
p@data <- dplyr::left_join(p@data, d, by = "Name")
tributaries <- p
save(tributaries, file = file.path(dir.out, "tributaries.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("tributaries")

## ----precip_zones_1------------------------------------------------------
ids <- c("Ketchum", "Hailey", "Picabo")
y <- coordinates(weather.stations)[match(rev(ids), weather.stations@data$id), "y"]
s <- c(mean(y[1:2]), mean(y[2:3]))  # northing zone separators
e <- extend(ext, 5000)  # spatial extent of zones
p1 <- rbind(c(e[1], s[2]), c(e[2], s[2]), c(e[2], e[4]), c(e[1], e[4]))
p2 <- rbind(c(e[1], s[1]), c(e[2], s[1]), c(e[2], s[2]), c(e[1], s[2]))
p3 <- rbind(c(e[1], e[3]), c(e[2], e[3]), c(e[2], s[1]), c(e[1], s[1]))
p1 <- Polygons(list(Polygon(rbind(p1, p1[1, ]))), ID = 1)
p2 <- Polygons(list(Polygon(rbind(p2, p2[1, ]))), ID = 2)
p3 <- Polygons(list(Polygon(rbind(p3, p3[1, ]))), ID = 3)
p <- SpatialPolygons(list(p1, p2, p3), proj4string = crs)
p <- SpatialPolygonsDataFrame(p, data.frame(ID = 1:3, PrecipZone = ids))
precip.zones <- p
save(precip.zones, file = file.path(dir.out, "precip.zones.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("precip.zones")

## ----irr_entities_1------------------------------------------------------
path <- file.path(dir.in, "irr")
irr.entities <- readOGR(path, "irr.entities", verbose = FALSE)
irr.entities <- spTransform(irr.entities, crs)
irr.entities <- rgeos::gBuffer(irr.entities, width = 0, byid = TRUE)
d <- irr.entities@data
d$EntitySrce <- as.factor(paste(d$EntityName, d$Source))
d$PrecipZone <- over(rgeos::gCentroid(irr.entities, byid = TRUE), precip.zones)$PrecipZone
irr.entities@data <- d[, c("EntityName", "Source", "EntitySrce", "PrecipZone")]
save(irr.entities, file = file.path(dir.out, "irr.entities.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("irr.entities")

## ----irr_lands_1---------------------------------------------------------
path <- file.path(dir.in, "irr")
yr <- c(1996, 2000, 2002, 2006, 2008, 2009, 2010)
files <- paste0("irr.lands.", yr)
irr.lands <- list()
for (i in seq_along(files)) {
  p <- readOGR(path, files[i], verbose = FALSE)
  p <- spTransform(p, crs)
  p@data <- p@data[, paste0("STATUS_", substr(yr[i], 1, 3)), drop = FALSE]
  names(p@data) <- "Status"
  p <- p[p@data[, "Status"] != "non-irrigated", ]
  p <- rgeos::gBuffer(p, width = 0, byid = TRUE)
  p@data <- droplevels(p@data)
  irr.lands[[i]] <- p
}
names(irr.lands) <- as.character(yr)
save(irr.lands, file = file.path(dir.out, "irr.lands.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("irr.lands")

## ----lakes_1-------------------------------------------------------------
path <- file.path(dir.in, "decorative")
lakes <- readOGR(path, "lakes", verbose = FALSE)
lakes <- spTransform(lakes, crs)
save(lakes, file = file.path(dir.out, "lakes.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("lakes")

## ----public_parcels_1----------------------------------------------------
public.parcels <- readOGR(dir.in, "public.parcels", verbose = FALSE)
public.parcels <- spTransform(public.parcels, crs)
public.parcels <- as(public.parcels, "SpatialPolygons")
save(public.parcels, file = file.path(dir.out, "public.parcels.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("public.parcels")

## ----soils_1-------------------------------------------------------------
soils <- readOGR(dir.in, "soils", verbose = FALSE)
soils <- spTransform(soils, crs)
soils@data <- soils@data[, c("GroupSym", "SoilLayer")]
names(soils@data)[1] <- "GroupSymbol"
file <- file.path(dir.in, "soils.csv")
d <- read.csv(file, strip.white = TRUE)
d$MinRate         <- d$MinRate_ft.per.mo * ft.to.m
d$MaxRate         <- d$MaxRate_ft.per.mo * ft.to.m
d$PercolationRate <- d$Rate_ft.per.mo    * ft.to.m
d <- d[, c("GroupSymbol", "SoilClass", "MinRate", "MaxRate", "PercolationRate")]
d <- suppressWarnings(dplyr::left_join(soils@data, d, by = "GroupSymbol"))
d$GroupSymbol <- as.factor(d$GroupSymbol)
d$PercolationRate[is.na(d$PercolationRate)] <- 0
soils@data <- d
save(soils, file = file.path(dir.out, "soils.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("soils")

## ----wetlands_1----------------------------------------------------------
wetlands <- readOGR(dir.in, "wetlands", verbose = FALSE)
wetlands <- spTransform(wetlands, crs)
wetlands <- as(wetlands, "SpatialPolygons")
save(wetlands, file = file.path(dir.out, "wetlands.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("wetlands")

## ----bellevue_wwtp_ponds_1-----------------------------------------------
bellevue.wwtp.ponds <- readOGR(dir.in, "bellevue.wwtp.ponds", verbose = FALSE)
bellevue.wwtp.ponds <- spTransform(bellevue.wwtp.ponds, crs)
bellevue.wwtp.ponds <- as(bellevue.wwtp.ponds, "SpatialPolygons")
save(bellevue.wwtp.ponds, file = file.path(dir.out, "bellevue.wwtp.ponds.rda"),
     compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("bellevue.wwtp.ponds")

## ----kriging_zones_1-----------------------------------------------------
path <- file.path(dir.in, "opt")
kriging.zones <- readOGR(path, "kriging.zones", verbose = FALSE)
kriging.zones <- spTransform(kriging.zones, crs)
save(kriging.zones, file = file.path(dir.out, "kriging.zones.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("kriging.zones")

## ----idaho_1-------------------------------------------------------------
path <- file.path(dir.in, "decorative")
idaho <- readOGR(path, "idaho", verbose = FALSE)
idaho <- as(idaho, "SpatialPolygons")
idaho <- spTransform(idaho, crs)
save(idaho, file = file.path(dir.out, "idaho.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("idaho")

## ----alluvium_thickness_1------------------------------------------------
file <- file.path(dir.in, "alluvium.thickness.tif")
alluvium.thickness <- readGDAL(file, band = 1, silent = TRUE)
alluvium.thickness <- projectRaster(raster(alluvium.thickness), high.res.spatial.grid)
alluvium.thickness <- aggregate(alluvium.thickness, fact = 5L, fun = median)
names(alluvium.thickness) <- "alluvium.thickness"
save(alluvium.thickness, file = file.path(dir.out, "alluvium.thickness.rda"),
     compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("alluvium.thickness")

## ----land_surface_1------------------------------------------------------
path <- file.path(dir.in, "grdn44w115_13")
r <- raster(readGDAL(path, band = 1, silent = TRUE))
land.surface <- projectRaster(r, high.res.spatial.grid)
land.surface <- aggregate(land.surface, fact = 5L, fun = median)
land.surface[is.na(alluvium.thickness)] <- NA
names(land.surface) <- "land.surface"
save(land.surface, file = file.path(dir.out, "land.surface.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("land.surface")

## ----hill_shading_1------------------------------------------------------
ext <- extent(spatial.grid)
ext <- extent(c(extendrange(c(ext@xmin, ext@xmax), f = 0.05),
                extendrange(c(ext@ymin, ext@ymax), f = 0.05)))
nrows <- (ext@ymax - ext@ymin) / 20
ncols <- (ext@xmax - ext@xmin) / 20
r <- projectRaster(r, raster(ext, nrows = nrows, ncols = ncols, crs = crs))
r[] <- r[] * 2
r <- hillShade(slope = terrain(r, opt = "slope"), aspect = terrain(r, opt = "aspect"))
r.range <- range(r[], na.rm = TRUE)
r[] <- findInterval(r[], seq(r.range[1], r.range[2], length.out = 255)) / 255
r[] <- round(r[], digits = 6)
hill.shading <- r
save(hill.shading, file = file.path(dir.out, "hill.shading.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("hill.shading")
rm(hill.shading)

## ----et_1----------------------------------------------------------------
files <- file.path(dir.in, "et", paste0("et.", yr.mo, ".tif"))
FUN <- function(i) {
  r <- readGDAL(files[i], band = 1, silent = TRUE)
  r[[1]] <- r[[1]] * mm.to.m
  return(r)
}
et.raw <- lapply(seq_along(files), FUN)
names(et.raw) <- as.character(yr.mo)

## ----et_2----------------------------------------------------------------
is.missing <- is.na(alluvium.thickness)
FUN <- function(i) {
  r <- aggregate(projectRaster(raster(i), high.res.spatial.grid), fact = 5L)
  r[is.missing] <- NA
  upper.limit <- mean(r[], na.rm = TRUE) + sd(r[], na.rm = TRUE) * 3
  r[r > upper.limit] <- upper.limit
  return(round(r, digits = 6))
}
et <- stack(lapply(et.raw, FUN), quick = TRUE)
names(et) <- as.character(yr.mo)
save(et, file = file.path(dir.out, "et.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("et")

## ----entity_components_1-------------------------------------------------
p <- irr.lands
p <- lapply(p, function(i) inlmisc::SetPolygons(i, wetlands, "gDifference", 0.001))
p <- lapply(p, function(i) inlmisc::SetPolygons(i, public.parcels, "gDifference", 0.001))
p <- lapply(p, function(i) inlmisc::SetPolygons(irr.entities, i, "gIntersection", 0.001))
for (i in seq_along(p)) p[[i]]@data$area <- rgeos::gArea(p[[i]], byid = TRUE)
irr.by.entity <- p

## ----echo=FALSE----------------------------------------------------------
rm(list=c("irr.lands", "wetlands", "public.parcels"))

## ----entity_components_2-------------------------------------------------
FUN <- function(i) {
  d <- aggregate(i@data$area, by = list(i@data$EntitySrce), sum, na.rm = TRUE)
  names(d) <- c("EntitySrce", "area")
  FUN <- function(j) as.character(i@data$PrecipZone[i@data$EntitySrce == j][1])
  d$PrecipZone <- as.factor(vapply(d$EntitySrce, FUN, ""))
  return(d)
}
area.by.entity <- lapply(irr.by.entity, FUN)

## ----entity_components_3-------------------------------------------------
FUN <- function(i) {
  yr <- irr.lands.year$IL_Year[irr.lands.year$Year %in% substr(i, 1, 4)]
  p <- irr.by.entity[[yr]]
  unique.sources <- sort(unique(as.character(p@data$EntitySrce)))
  FUN <- function (j) {
    x <- rgeos::gUnaryUnion(p[p@data$EntitySrce == j, ])@polygons[[1]]
    slot(x, "ID") <- j
    return(x)
  }
  sp <- SpatialPolygons(lapply(unique.sources, FUN), proj4string = crs(et.raw[[i]]))
  p <- over(sp, et.raw[[i]], fn = mean, na.rm = TRUE)
  d <- as.data.frame(list(EntitySrce = rownames(p), mean.et = p[, 1]))
  d <- suppressWarnings(dplyr::left_join(d, area.by.entity[[yr]], by = "EntitySrce"))
  d$et.vol <- d$mean.et * d$area
  d$precip.vol <- NA
  for (j in levels(d$PrecipZone)) {
    is.in.zone <- d$PrecipZone == j
    idx <- which(precipitation$YearMonth == i & precipitation$PrecipZone == j)
    d$precip.vol[is.in.zone] <- d$area[is.in.zone] * precipitation$Precip[idx]
  }
  d$cir.vol <- d$et.vol - d$precip.vol
  idxs <- match(d$EntitySrce, irr.entities@data$EntitySrce)
  d[, c("EntityName", "Source")] <- irr.entities@data[idxs, c("EntityName", "Source")]
  rownames(d) <- d$EntitySrce
  return(SpatialPolygonsDataFrame(sp, d))
}
entity.components <- lapply(yr.mo, FUN)
names(entity.components) <- yr.mo
save(entity.components, file = file.path(dir.out, "entity.components.rda"),
     compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("entity.components")
rm(et.raw)

## ----r_canals_1----------------------------------------------------------
r <- rasterize(canals, land.surface, "EntityName", silent = TRUE)
r <- ratify(r, count = TRUE)
d <- levels(r)[[1]]
d$EntityName <- levels(canals@data$EntityName)[d$ID]
levels(r) <- d
r.canals <- r
save(r.canals, file = file.path(dir.out, "r.canals.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("r.canals")

## ----rs_entities_1-------------------------------------------------------
FUN <- function(i) {
  r <- rasterize(entity.components[[i]], land.surface, "EntityName", silent = TRUE)
  r <- ratify(r, count = TRUE)
  d <- levels(r)[[1]]
  d$EntityName <- levels(entity.components[[i]]@data$EntityName)[d$ID]
  levels(r) <- d
  return(r)
}
rs.entities <- stack(lapply(yr.mo, FUN), quick = TRUE)
names(rs.entities) <- yr.mo
save(rs.entities, file = file.path(dir.out, "rs.entities.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("rs.entities")

## ----rs_rech_non_irr_1---------------------------------------------------
r.zones <- ratify(rasterize(precip.zones, land.surface, "ID", silent = TRUE))
levels(r.zones) <- cbind(levels(r.zones)[[1]], att = precip.zones@data$PrecipZone)
r.soils <- rasterize(soils, land.surface, "PercolationRate", silent = TRUE)
cell.area <- xres(land.surface) * yres(land.surface)
FUN <- function(i) {
  p <- precipitation[precipitation$YearMonth == i, c("PrecipZone", "Precip")]
  p <- p[match(p$PrecipZone, levels(r.zones)[[1]]$att), "Precip"]
  names(p) <- levels(r.zones)[[1]]$att
  r <- r.zones
  levels(r) <- cbind(levels(r)[[1]], Precip = p)
  r <- deratify(r, "Precip") - et[[i]]
  is.pos <- r > r.soils
  r[is.pos] <- r.soils[is.pos]
  if (i %in% yr.mo.irr) r[!is.na(rs.entities[[i]])] <- NA
  r <- r * cell.area
  r[] <- round(r[], digits = 6)
  return(r)
}
rs.rech.non.irr <- stack(lapply(yr.mo, FUN), quick = TRUE)
names(rs.rech.non.irr) <- yr.mo
save(rs.rech.non.irr, file = file.path(dir.out, "rs.rech.non.irr.rda"), compress = "xz")

## ----echo=FALSE----------------------------------------------------------
CheckStatus("rs.rech.non.irr")

