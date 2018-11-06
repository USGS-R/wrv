#' Extent of Alluvium Unit
#'
#' Estimated extent of alluvium unit in the Wood River Valley aquifer system.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 1 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Extent defined by Bartollino and Adkins (2012, Plate 1).
#'
#' @references Bartolino, J.R., and Adkins, C.B., 2012,
#'   Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p.,
#'   available at \url{https://pubs.usgs.gov/sir/2012/5053/}.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(alluvium.extent, col = "#BFA76F")
#' str(alluvium.extent)
#'
"alluvium.extent"

#' Thickness of the Quaternary Sediment
#'
#' Estimated thickness of the Quaternary sediment in the Wood River Valley aquifer system.
#'
#' @format An object of RasterLayer class.
#'   Each cell on the surface grid represents a depth measured from land surface in meters.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'   The spatial grid is composed of 565 rows and 429 columns,
#'   and has cell sizes that are constant at 100 meters by 100 meters.
#'
#' @source Revised version of Plate 1 in Bartolino and Adkins (2012).
#'
#' @references Bartolino, J.R., and Adkins, C.B., 2012,
#'   Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p.,
#'   available at \url{https://pubs.usgs.gov/sir/2012/5053/}.
#'
#' @keywords datasets
#'
#' @examples
#' col <- rainbow(255, start = 0.0, end = 0.8)
#' raster::image(alluvium.thickness, col = col, asp = 1, axes = FALSE,
#'               xlab = "", ylab = "")
#' summary(alluvium.thickness)
#'
"alluvium.thickness"

#' Extent of Basalt Unit
#'
#' Estimated extent of the basalt unit underlying the alluvial Wood River Valley aquifer system.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 1 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Extent defined by Bartollino and Adkins (2012, Plate 1).
#'
#' @references Bartolino, J.R., and Adkins, C.B., 2012,
#'   Hydrogeologic framework of the Wood River Valley aquifer system, south-central Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p.,
#'   available at \url{https://pubs.usgs.gov/sir/2012/5053/}.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(basalt.extent, col = "#BEAED4", border = NA)
#' sp::plot(alluvium.extent, add = TRUE)
#' str(basalt.extent)
#'
"basalt.extent"

#' Bellevue Waste Water Treatment Plant Ponds
#'
#' Location of the Bellevue Waste Water Treatment Plant ponds.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 1 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on December 11, 2014
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(bellevue.wwtp.ponds)
#'
"bellevue.wwtp.ponds"

#' Bypass Canal
#'
#' Location of the Bypass Canal in the Wood River Valley.
#'
#' @format An object of SpatialLines class containing 4 Lines.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on January 15, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(bypass.canal)
#'
"bypass.canal"

#' Canal Seepage
#'
#' Canal seepage as a fraction of diversions for irrigation entities in the Wood River Valley.
#'
#' @format An object of class data.frame with 19 records and the following variables:
#'   \describe{
#'     \item{EntityName}{name of the irrigation entity served by the canal system.}
#'     \item{SeepFrac}{estimated canal seepage as a fraction of diversions.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on November 4, 2015
#'
#' @seealso \code{\link{canals}}
#'
#' @keywords datasets
#'
#' @examples
#' str(canal.seep)
#'
#' d <- canal.seep[order(canal.seep$SeepFrac, decreasing=TRUE), ]
#' par(mar = c(4.1, 8.1, 0.1, 0.6))
#' barplot(d$SeepFrac, names.arg = d$EntityName, horiz = TRUE, cex.names = 0.7,
#'         cex.axis = 0.7, cex.lab = 0.7, las = 1, xlab = "Seepage fraction")
#'
#' graphics.off()
#'
"canal.seep"

#' Canal Systems
#'
#' Canal systems in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialLinesDataFrame class containing
#'   113 Lines and a data.frame with the following variable:
#'   \describe{
#'     \item{EntityName}{name of the irrigation entity served by the canal system.}
#'     \item{Name}{local canal name}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on November 29, 2014
#'
#' @seealso \code{\link{r.canals}}, \code{\link{canal.seep}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(canals, col = "#3399CC")
#' str(canals@data)
#'
"canals"

#' Cities and Towns
#'
#' Cities and towns in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialPointsDataFrame class containing 11 points.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources
#'   (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}),
#'   accessed on April 15, 2015
#'
#' @keywords datasets
#'
#' @examples
#' str(cities)
#'
#' col <- "#333333"
#' sp::plot(cities, pch = 15, cex = 0.8, col = col)
#' text(cities, labels = cities@data$FEATURE_NA, col = col, cex = 0.5, pos = 1, offset = 0.4)
#'
"cities"

#' Extent of Clay Unit
#'
#' Estimated extent of the clay confining unit (aquitard) separating the
#' unconfined aquifer from the underlying confined aquifer in the
#' Wood River Valley aquifer system.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 2 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Extent defined by Moreland (1977, fig. 3 in USGS Open-File report).
#'   Moreland (1977) shows an outlier by Picabo that is assumed to
#'   indicate confined conditions in the basalt and not the lake sediments.
#'
#' @references Moreland, J.A., 1977, Ground water-surface water relations in the Silver Creek area,
#'   Blaine County, Idaho: Boise, Idaho Department of Water Resources, Water Information Bulletin 44,
#'   42 p., 5 plates in pocket, accessed January 31, 2012.
#'   Also published as U.S. Geological Survey Open-File report 77-456, 66 p.,
#'   available at \url{https://pubs.er.usgs.gov/publication/ofr77456}.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(clay.extent, col = "#FDC086", border = NA)
#' sp::plot(alluvium.extent, add = TRUE)
#' str(clay.extent)
#'
"clay.extent"

#' Combined Surface-Water Irrigation Diversions
#'
#' Supplemental groundwater rights and associated surface-water rights.
#'
#' @format An object of class data.frame with 1,213 records and the following variables:
#'   \describe{
#'     \item{WaterRight}{name of the supplemental groundwater right.}
#'     \item{CombWaterRight}{name of the surface-water right that shares a
#'       combined limit with the groundwater right.}
#'     \item{Source}{river or stream source name for the surface-water right.}
#'     \item{WaterUse}{authorized beneficial use for the surface-water right.}
#'     \item{MaxDivRate}{authorized maximum diversion rate for the surface-water right,
#'       in cubic meters per day.}
#'     \item{Pdate}{priority date of the surface-water right.}
#'   }
#'
#' @source Idaho Department of Water Resources (IDWR), accessed on April 25, 2014;
#'   derived from combined limit comments in IDWR water rights database.
#'
#' @keywords datasets
#'
#' @examples
#' str(comb.sw.irr)
#'
"comb.sw.irr"

#' Groundwater Diversions
#'
#' Groundwater diversions recorded by Water District 37 or municipal water providers.
#' Groundwater is diverted from the aquifer by means of either pumping wells or
#' flowing-artesian wells.
#'
#' @format An object of class data.frame with 7,292 records and the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month during which diversions were recorded,
#'       with a required date format of YYYYMM.}
#'     \item{Diversion}{name of the well}
#'     \item{Reach}{name of the river subreach into which the well water is discharged;
#'       only applicable to exchange wells.}
#'     \item{BigReach}{name of the river reach into which the well water is discharged;
#'       only applicable to exchange wells.}
#'     \item{EntityName}{name of the irrigation entity which the well supplies water.}
#'     \item{WMISNumber}{well number in the Idaho Department of Water Resources (IDWR)
#'       Water Measurement Information System.}
#'     \item{GWDiv}{volume of water diverted during the month, in cubic meters.}
#'   }
#'
#' @source Idaho Department of Water Resources (IDWR), accessed on December 11, 2014;
#'   compiled data records from Water District 37 and 37M, City of Ketchum,
#'   Sun Valley Water and Sewer District, City of Hailey, and City of Bellevue.
#'
#' @keywords datasets
#'
#' @examples
#' str(div.gw)
#'
"div.gw"

#' Diversions, Returns, and Exchange Wells
#'
#' Location of streamflow diversions, irrigation canal or pond returns, and
#' exchange well returns.
#'
#' @format An object of SpatialPointsDataFrame class containing 117 points with the
#'   following variables:
#'   \describe{
#'     \item{Name}{local name for the diversion/return site.}
#'     \item{Type}{data type, either \dQuote{Diversion}, \dQuote{Return}, or
#'       \dQuote{Exchange well inflow}.}
#'     \item{LocSource}{data source}
#'     \item{Big}{corresponding river reach}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on June 5, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(div.ret.exch)
#' str(div.ret.exch@data)
#'
"div.ret.exch"

#' Surface-Water Diversions
#'
#' Surface-water diversions recorded by Water District 37 or municipal water providers.
#'
#' @format An object of class data.frame with 15,550 records and the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month during which diversions were recorded,
#'       with a required date format of YYYYMM.}
#'     \item{Diversion}{name of the surface-water diversion.}
#'     \item{Reach}{river subreach from which the water is diverted.}
#'     \item{BigReach}{river reach from which the water is diverted.}
#'     \item{EntityName}{name of the irrigation entity which the diversion supplies water.}
#'     \item{SWDiv}{volume of water diverted during the month, in cubic meters.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on December 11, 2014;
#'   compiled data records from Water District 37 and 37M, City of Hailey,
#'   City of Bellevue, City of Ketchum, and Sun Valley Water and Sewer District.
#'
#' @keywords datasets
#'
#' @examples
#' str(div.sw)
#'
"div.sw"

#' Wastewater Treatment Plant Diversions
#'
#' Discharge from wastewater treatment plants.
#'
#' @format An object of class data.frame with 1,182 records and the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month during which diversions were recorded,
#'       with a required date format of YYYYMM.}
#'     \item{Return}{name of the wastewater treatment plant.}
#'     \item{Reach}{name of the river subreach to which treated effluent is discharged;
#'       only applicable to wastewater treatment plants that discharge to the river.}
#'     \item{BigReach}{name of the river reach to which treated effluent is discharged;
#'       only applicable to wastewater treatment plants that discharge to the river.}
#'     \item{EntityName}{name of the irrigation entity served by the wastewater treatment plant.}
#'     \item{WWDiv}{volume of wastewater discharged during the month, in cubic meters.}
#'   }
#'
#' @source Idaho Department of Water Resources and U.S. Geological Survey,
#'   accessed on August 11, 2014; compiled data records from the
#'   U.S. Environmental Protection Agency for plants that discharge to the river,
#'   and from records of the Idaho Department of Environmental Quality for
#'   plants that discharge to land application.
#'
#' @keywords datasets
#'
#' @examples
#' str(div.ww)
#'
"div.ww"

#' Drain Boundaries at Stanton Crossing and Silver Creek
#'
#' Polygons used to define the locations of drain boundaries in the model domain.
#' The polygons clip the line segments along the aquifer boundary (see \code{\link{alluvium.extent}}),
#' and model cells intersecting these clipped-line segments are defined as boundary cells.
#'
#' @format An object of SpatialPolygonsDataFrame class containing a set of 2 Polygons and
#'   a data.frame with the following variable:
#'   \describe{
#'     \item{Name}{identifier for the polygon.}
#'     \item{cond}{drain conductance in square meters per day.}
#'     \item{elev}{drain threshold elevation in meters above the
#'       North American Vertical Datum of 1988 (NAVD 88).}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source U.S. Geological Survey, accessed on March 27, 2015;
#'   a Keyhole Markup Language (\href{https://en.wikipedia.org/wiki/Kml}{KML}) file created in
#'   \href{https://www.google.com/earth/}{Google Earth} with polygons drawn by hand in
#'   areas of known drains.
#'
#' @keywords datasets
#'
#' @examples
#' str(drains)
#'
#' sp::plot(drains, border = "red")
#' sp::plot(alluvium.extent, add = TRUE)
#'
"drains"

#' Dry River Bed and Stream Fed Creek Conditions
#'
#' A summary of dry river bed and stream fed conditions in the Wood River Valley, Idaho.
#' Stream reaches on the Big Wood River between Glendale and Wood River Ranch are episodically dry;
#' these dry periods are specified for calendar months when water diversions to the
#' Bypass Canal begins before the 16th of the month and ends after the 15th of the month.
#'
#' @format An object of data.frame class with 12 records and the following variables:
#'   \describe{
#'     \item{Reach}{stream reach name}
#'     \item{199501,\dots,201012}{logical values indicating whether the stream reach exhibits
#'       dry-bed conditions during a stress period.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on January 6, 2016;
#'   compiled from Water District 37 records.
#'
#' @keywords datasets
#'
#' @examples
#' str(drybed)
#'
"drybed"

#' Irrigation Efficiency
#'
#' The irrigation efficiency of irrigation entities in the Wood River Valley, Idaho.
#'
#' @format An object of data.frame class with 88 records and the following variables:
#'   \describe{
#'     \item{EntityName}{name of the irrigation entity which the irrigation efficiency is applied.}
#'     \item{Eff}{estimated irrigation efficiency, the ratio of the amount of
#'       water consumed by the crop to the amount of water supplied through irrigation.}
#'     \item{Comment}{brief comment on irrigation conditions.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on July 9, 2015
#'
#' @keywords datasets
#'
#' @examples
#' str(efficiency)
#'
"efficiency"

#' Irrigation Entity Components
#'
#' Irrigation entities and their components in the Wood River Valley and surrounding areas.
#' An irrigation entity is defined as an area served by a group of surface-water and/or
#' groundwater diversion(s).
#'
#' @format An object of list class with components of SpatialPolygonsDataFrame-class.
#'   There are a total of 192 components, one for each month in the 1995--2010 time period.
#'   Linked data.frame objects have the following variables:
#'   \describe{
#'     \item{EntitySrce}{concatenation of the \code{EntityName} and \code{Source} character strings.}
#'     \item{mean.et}{mean evapotranspiration (ET) on irrigated and semi-irrigated lands in meters.}
#'     \item{area}{area of irrigated and semi-irrigated lands in square meters.}
#'     \item{PrecipZone}{name of the precipitation zone;
#'       see \code{\link{precip.zones}} dataset for details.}
#'     \item{et.vol}{volume of ET on irrigated and semi-irrigated lands in cubic meters.}
#'     \item{precip.vol}{volume of precipitation on irrigated and semi-irrigated lands in cubic meters.}
#'     \item{cir.vol}{volume of crop irrigation requirement in cubic meters (ET minus precipitation).}
#'     \item{EntityName}{name of the irrigation entity.}
#'     \item{Source}{water source, either \dQuote{Mixed} for a mixture of surface water and groundwater,
#'       \dQuote{SW Only} for surface water only, or \dQuote{GW Only} for groundwater only.}
#'   }
#'
#' @source Calculated from the \code{\link{irr.entities}}, \code{\link{wetlands}},
#'   \code{\link{public.parcels}}, \code{\link{irr.lands.year}}, \code{\link{et}}, and
#'   \code{\link{precipitation}} datasets;
#'   see the \file{package-datasets} vignette for the \R code used in this calculation.
#'
#' @keywords datasets
#'
#' @examples
#' names(entity.components)
#' sp::plot(entity.components[["199506"]])
#' print(entity.components[["199506"]])
#'
"entity.components"

#' Evapotranspiration
#'
#' Evapotranspiration (ET) in the Wood River Valley and surrounding areas.
#' Defined as the amount of water lost to the atmosphere via direct evaporation,
#' transpiration by vegetation, or sublimation from snow covered areas.
#'
#' @format An object of RasterStack class containing 192 RasterLayer objects,
#'   one layer for each month in the 1995-2010 time period.
#'   Each cell on a layers surface grid represents the monthly depth of ET in meters.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on November 17, 2014
#'
#' @seealso \code{\link{et.method}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(et[["199505"]])
#' print(et)
#'
"et"

#' Method Used to Calculate Evapotranspiration
#'
#' Methods used to estimate monthly distributions of evapotranspiration (ET) rate.
#'
#' @format An object of data.frame class with 122 records with the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month during which the method was applied,
#'       with a required date format of YYYYMM.}
#'     \item{ETMethod}{Identifier that indicates the method used to estimate ET values.
#'       Identifiers include either
#'         \dQuote{Allen-Robison}, the Allen and Robison method (Allen and Robison, 2007);
#'         \dQuote{METRIC}, the Mapping Evapotranspiration at high Resolution and with
#'           Internalized Calibration (METRIC) model (Allen and others, 2010a);
#'         \dQuote{NDVI}, the Normalized Difference Vegetation Index (NDVI) method
#'           (Allen and others, 2010b);
#'         \dQuote{Interpolation}, interpolation from known ET data; or
#'         \dQuote{METRIC-NDVI}, a combination of METRIC and NDVI methods.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on April 27, 2015
#'
#' @references Allen, R., and Robison, C.W., 2007, Evapotranspiration and
#'   consumptive water requirements for Idaho, University of Idaho, Kimberly, Idaho.
#'
#'   Allen, R., Tasumi, M., Trezza, R., and Kjaersgaard, J., 2010a,
#'     METRIC mapping evapotranspiration at high resolution applications manual for
#'     Landsat satellite imagery version 2.07, University of Idaho, Kimberly, ID.
#'
#'   Allen, R., Robison, C.W., Garcia, M., Trezza, R., Tasumi, M., and Kjaersgaard, J., 2010b,
#'     ETrF vs NDVI relationships for southern Idaho for rapid estimation of evapotranspiration,
#'     University of Idaho, Kimberly, ID.
#'
#'   ET Idaho: \url{http://data.kimberly.uidaho.edu/ETIdaho/}
#'
#' @keywords datasets
#'
#' @examples
#' str(et.method)
#'
"et.method"

#' Daily Mean Discharge at Streamgages
#'
#' The daily mean discharge at streamgages in the Big Wood River Valley, Idaho.
#' Discharge records bracket the 1992-2014 time period and are based on
#' records with quality assurance code of approved (\sQuote{A}).
#'
#' @format An object of data.frame class with 8,315 records and the following variables:
#'   \describe{
#'     \item{Date}{date during which discharge was averaged.}
#'     \item{13135500}{daily mean discharge in cubic meters per day, recorded at the USGS
#'       \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13135500}{13135500}
#'       Big Wood River near Ketchum streamgage.}
#'     \item{13139510}{daily mean discharge in cubic meters per day, recorded at the USGS
#'       \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13139510}{13139510}
#'       Big Wood River at Hailey streamgage.}
#'     \item{13140800}{daily mean discharge in cubic meters per day, recorded at the USGS
#'       \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13140800}{13140800}
#'       Big Wood River at Stanton Crossing near Bellevue streamgage.}
#'   }
#'
#' @source National Water Information System (\href{https://waterdata.usgs.gov/nwis}{NWIS}),
#'   accessed on January 8, 2015
#'
#' @keywords datasets
#'
#' @examples
#' str(gage.disch)
#'
#' col <- c("red", "blue", "green")
#' ylab <- paste("Discharge in cubic", c("meters per day", "acre-foot per year"))
#' inlmisc::PlotGraph(gage.disch, ylab = ylab, col = col, lty = 1:3,
#'                    conversion.factor = 0.29611, seq.date.by = "year")
#' leg <- sprintf("USGS \%s", names(gage.disch)[-1])
#' legend("topright", leg, col = col, lty = 1:3, inset = 0.02, cex = 0.7,
#'        box.lty = 1, bg = "#FFFFFFE7")
#'
#' graphics.off()
#'
"gage.disch"

#' Daily Mean Gage Height at Streamgages
#'
#' The daily mean gage height at streamgages in the Big Wood River Valley, Idaho.
#' Gage height records bracket the 1987-2014 and are based on records with
#' quality assurance codes of working (\sQuote{W}), in review (\sQuote{R}), and
#' approved (\sQuote{A}).
#'
#' @format An object of data.frame class with 9,980 records and the following variables:
#'   \describe{
#'     \item{Date}{date during which gage height was averaged.}
#'     \item{13135500}{daily mean gage height in meters, recorded at the USGS
#'       \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13135500}{13135500}
#'       Big Wood River near Ketchum streamgage.}
#'     \item{13139510}{daily mean gage height in meters, recorded at the USGS
#'       \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13139510}{13139510}
#'       Big Wood River at Hailey streamgage.}
#'     \item{13140800}{daily mean gage height in meters, recorded at the USGS
#'       \href{https://waterdata.usgs.gov/id/nwis/uv/?site_no=13140800}{13140800}
#'       Big Wood River at Stanton Crossing near Bellevue streamgages.}
#'   }
#'
#' @source Data queried from the National Water Information System
#'   (\href{https://waterdata.usgs.gov/nwis}{NWIS}) database on December 15, 2014,
#'   by Ross Dickinson (USGS).
#'   Records recorded on May 26-28, 1991 and March 15-22, 1995 were reassigned
#'   quality assurance codes of \sQuote{I} because of assumed ice build-up.
#'   Missing data at the Big Wood River near Ketchum gage was estimated using a
#'   linear regression model based on recorded gage-height data at the Big Wood River at
#'   Hailey and Near Ketchum streamgages.
#'   Missing data at the Stanton Crossing near Bellevue gage was replaced with
#'   average gage-height values recorded at this gage.
#'
#' @keywords datasets
#'
#' @examples
#' str(gage.height)
#'
#' col <- c("red", "blue", "green")
#' ylab <- paste("Gage height in", c("meters", "feet"))
#' inlmisc::PlotGraph(gage.height, ylab = ylab, col = col, lty = 1:3,
#'                    conversion.factor = 3.28084, seq.date.by = "year")
#' leg <- sprintf("USGS \%s", names(gage.height)[-1])
#' legend("topright", leg, col = col, lty = 1:3, inset = 0.02, cex = 0.7,
#'        box.lty = 1, bg = "#FFFFFFE7")
#'
#' graphics.off()
#'
"gage.height"

#' Land Surface Hill Shading
#'
#' Hill shading of the Wood River Valley and surrounding area.
#'
#' @format An object of \code{RasterLayer} class.
#'   Each cell on the surface grid represents the hill shade.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'   The spatial grid is composed of 3,108 rows and 2,360 columns,
#'   and has cell sizes that are constant at 20 meters by 20 meters.
#'
#' @source Calculated from the slope and aspect of the \code{\link{land.surface}} dataset
#'   using the \code{terrain} and \code{hillShade} functions;
#'   see the appendix C. Package Dataset Creation for the \R code used in this calculation.
#'
#' @keywords datasets
#'
#' @examples
#' raster::image(hill.shading, length(hill.shading), col = grey(0:255 / 255), asp = 1,
#'               axes = FALSE, xlab = "", ylab = "")
#'
"hill.shading"

#' U.S. State of Idaho
#'
#' Boundary of Idaho, a state in the northwestern region of the United States.
#'
#' @format An object of SpatialPolygons class containing 1 Polygons.
#'   Cartographic boundary at 5m (1:5,000,000) resolution.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source U.S. Department of Commerce, U.S. Census Bureau,
#'   Geography Division/Cartographic Products Branch.
#'   A simplified representation of the State of Idaho from the 2014 Census Bureau's
#'   MAF/\href{https://www.census.gov/geo/maps-data/data/tiger.html}{TIGER} geographic database.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(idaho, col = "#EAE2CF", border = "#BFA76F", lwd = 0.5)
#' print(idaho)
#'
"idaho"

#' Irrigation Entities
#'
#' Delineation of areas served by a group of surface-water and (or) groundwater diversions.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 235 Polygons and
#'   a data.frame with the following variables:
#'   \describe{
#'     \item{EntityName}{name of the irrigation entity served by a group of diversions.}
#'     \item{Source}{water source, either \dQuote{Mixed} for a mixture of surface water and groundwater,
#'       \dQuote{SW Only} for surface-water only, or \dQuote{GW Only} for groundwater only.}
#'     \item{EntitySrce}{concatenation of the \code{EntityName} and \code{Source} character strings.}
#'     \item{PrecipZone}{name of the precipitation zone.
#'       See \code{\link{precip.zones}} dataset for details.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources (IDWR), accessed on December 11, 2014;
#'   derived from IDWR water rights database, Blaine County tax lot data,
#'   and IDWR irrigated land classification files.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(irr.entities)
#' print(irr.entities)
#'
"irr.entities"

#' Irrigated Lands
#'
#' Classification of irrigated lands in the Wood River Valley and surrounding areas;
#' available for years 1996, 2000, 2002, 2006, 2008, 2009, and 2010.
#'
#' @format An object of list class with 7 SpatialPolygonsDataFrame components.
#'   The data.frame associated with each SpatialPolygons object has the following variable:
#'   \describe{
#'     \item{Status}{status of land during the year reviewed,
#'       either \dQuote{irrigated}, \dQuote{semi-irrigated}, or \dQuote{non-irrigated}.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on November 17, 2014;
#'   polygons derived from U.S. Department of Agriculture Common Land Unit polygons
#'   with some refinement of polygons.
#'   Irrigation status interpreted using satellite imagery and aerial photography.
#'
#' @seealso \code{\link{irr.lands.year}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::spplot(irr.lands[["2010"]], "Status")
#' print(irr.lands)
#'
"irr.lands"

#' Irrigation Lands for a Given Year
#'
#' Annual land classification for irrigation practices is only available for select years.
#' For missing years, this dataset provides substitute years when land-classification was
#' available (see \code{\link{irr.lands}}).
#'
#' @format An object of data.frame class with 16 records and the following variables:
#'   \describe{
#'     \item{Year}{year with a required date format of YYYY.}
#'     \item{IL_Year}{substitute year with a required date format of YYYY.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on April 25, 2014
#'
#' @keywords datasets
#'
#' @examples
#' str(irr.lands.year)
#'
"irr.lands.year"

#' Kriging Zones
#'
#' Location of kriging zones in the Wood River Valley, used in parameter estimation.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 18 Polygons and a
#' data.frame with the following variables:
#'   \describe{
#'     \item{Zone}{kriging zone, interpolation in this zone is based on the
#'       parameter values of pilot points located within this zone.}
#'     \item{Layer}{model layer}
#'     \item{Name}{local name}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on June 11, 2015
#'
#' @seealso \code{\link{pilot.points}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(kriging.zones)
#' str(kriging.zones@data)
#'
"kriging.zones"

#' Lakes and Reservoirs
#'
#' Lakes and reservoirs of the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 55 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources
#'   (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}),
#'   accessed on April 2, 2014
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(lakes, col = "#CCFFFF", border = "#3399CC", lwd = 0.5)
#' str(lakes@data)
#'
"lakes"

#' Topography of Land Surface
#'
#' The Wood River Valley (WRV) is a geologic feature located in south-central Idaho.
#' This dataset gives the topography of the land surface in the WRV and vicinity.
#'
#' @format An object of SpatialGridDataFrame class.
#'   Each cell on the surface grid represents an elevation in meters above the
#'   North American Vertical Datum of 1988 (NAVD 88).
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'   The spatial grid is composed of 565 rows and 429 columns,
#'   and has cell sizes that are constant at 100 meters by 100 meters.
#'
#' @source The National Map (\href{https://nationalmap.gov/elevation.html}{TNM})
#'   1/3-arc-second raster (Gesch, 2007; Gesch and others, 2002),
#'   accessed on December 1, 2015.
#'   This dataset can be downloaded in a Esri ArcGRID format using the
#'   \href{https://viewer.nationalmap.gov/viewer/}{The National Map Viewer}.
#'   Elevation datasets are distributed in geographic coordinates in units of decimal degrees,
#'   and in conformance with the NAD 83.
#'   Elevation values are in meters above the NAVD 88.
#'   The west, east, south, and north bounding coordinates for this dataset are
#'   -115, -114, 43, and 44 decimal degrees, respectively.
#'   Post-processing includes:
#'     (1) project the values of the elevation dataset into the \code{\link{alluvium.thickness}}
#'         spatial grid using bilinear interpolation, and
#'     (2) set values in cells where the elevation of the alluvium bottom is missing to NA.
#'
#' @references Gesch, D.B., 2007, The National Elevation Dataset, in Maune, D., ed.,
#'   Digital Elevation Model Technologies and Applications: The DEM Users Manual,
#'   2nd Edition: Bethesda, Maryland, American Society for Photogrammetry and Remote Sensing,
#'   p. 99-118.
#'
#'   Gesch, D., Oimoen, M., Greenlee, S., Nelson, C., Steuck, M., and Tyler, D., 2002,
#'   The National Elevation Dataset: Photogrammetric Engineering and Remote Sensing,
#'   v. 68, no. 1, p. 5-11.
#'
#' @keywords datasets
#'
#' @examples
#' raster::image(land.surface)
#' summary(land.surface)
#'
"land.surface"

#' Major Roads
#'
#' Major roads in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialLinesDataFrame class containing 475 Lines.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources
#'   (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}),
#'   accessed on October 20, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(major.roads)
#' str(major.roads@data)
#'
"major.roads"

#' Map Labels
#'
#' Map labels in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialPointsDataFrame class containing
#'   40 points with the following variables:
#'   \describe{
#'     \item{label}{text to be written.}
#'     \item{cex}{character expansion factor}
#'     \item{col,font}{color and font to be used, respectively.}
#'     \item{srt}{string rotation in degrees.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Best estimates of map label locations.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(map.labels, col = "red")
#' lab <- cbind(map.labels@coords, map.labels@data)
#' for (i in seq_len(nrow(lab))) {
#'   text(lab$x[i], lab$y[i], labels = lab$label[i], cex = lab$cex[i],
#'        col = lab$col[i], font = lab$font[i], srt = lab$srt[i])
#' }
#'
"map.labels"

#' Miscellaneous Locations
#'
#' Miscellaneous locations in the Bellevue triangle area.
#'
#' @format An object of SpatialPointsDataFrame class containing 3 points
#'   with the following variable:
#'   \describe{
#'     \item{label}{description of point location.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources
#'   (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}),
#'   accessed on December 23, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(misc.locations, pch = 20, col = "red")
#' text(misc.locations, labels = misc.locations@data$label, pos = 3, cex = 0.6)
#'
"misc.locations"

#' Direct Recharge from Miscellaneous Seepage Sites
#'
#' Direct recharge from miscellaneous seepage sites in the Wood River Valley, Idaho.
#'
#' @format An object of data.frame class with 2 records and the following variables:
#'   \describe{
#'     \item{RechSite}{name of the recharge site, see \code{\link{bellevue.wwtp.ponds}} and
#'       \code{\link{bypass.canal}} datasets.}
#'     \item{199501,\dots,201012}{monthly volume of recharge during a stress period, in cubic meters.
#'       The variable name is specified as year and month.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on January 15, 2015
#'
#' @keywords datasets
#'
#' @examples
#' str(misc.seepage)
#'
"misc.seepage"

#' Observation Wells
#'
#' Observation wells in the Wood River Valley aquifer system.
#'
#' @format An object of SpatialPointsDataFrame class containing 776 points
#'   with the following variables:
#'   \describe{
#'     \item{id}{unique well identifier used in this study.}
#'     \item{SiteNo}{unique well identifier within the
#'       National Water Information System (NWIS).}
#'     \item{SITEIDIDWR}{unique well identifier within the
#'       Idaho Department of Water Resources (IDWR) hydrologic database.}
#'     \item{WELLNUMBER}{USGS or IDWR site name for the well.}
#'     \item{PESTNAME}{unique well identifier for PEST.}
#'     \item{METHODDRIL}{drilling method}
#'     \item{TOTALDEPTH}{depth at which drilling stopped, in feet.}
#'     \item{OPENINGMIN}{top of the screened interval, in feet.}
#'     \item{OPENINGMAX}{bottom of the screened interval, in feet.}
#'     \item{COMPLETION}{date on which the well drilling and construction stopped.}
#'     \item{WCWELLID}{well construction well identifier.}
#'     \item{ALTITUDE}{land surface elevation, in feet.}
#'     \item{ALTMETHOD}{method for obtaining the land surface elevation.}
#'     \item{XYMETHOD}{method of obtaining the spatial coordinates.}
#'     \item{BASINNO}{basin number}
#'     \item{COUNTYNAME}{Idaho county name}
#'     \item{TWPRGE}{township and range the well is located in.}
#'     \item{SITENAME}{local name for well.}
#'     \item{desc}{description of well type.}
#'     \item{TopOpen1}{depth to the top of the first open interval in a groundwater well,
#'       in meters below land surface.}
#'     \item{BotOpen1}{depth to the bottom of the first open interval in a groundwater well,
#'       in meters below land surface.}
#'     \item{TopOpen2}{not applicable}
#'     \item{BotOpen2}{not applicable}
#'   }
#'
#' @source Idaho Department of Water Resources well construction database,
#'   accessed on June 29, 2015
#'
#' @seealso \code{\link{obs.wells.head}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(obs.wells)
#' str(obs.wells@data)
#'
"obs.wells"

#' Hydraulic Heads in Observation Wells
#'
#' Hydraulic-head (groundwater-level) measurements recorded in observation wells in the
#' Wood River Valley aquifer system.
#' Values are used as observations during the parameter estimation process.
#'
#' @format An object of class data.frame with 3,477 records and the following variables:
#'   \describe{
#'     \item{PESTNAME}{unique well identifier for PEST.}
#'     \item{DateTime}{date and time when the measurement was recorded.}
#'     \item{Head}{groundwater-level measurement (hydraulic head)
#'       in meters above the North American Vertical Datum of 1988 (NAVD 88).}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on June 26, 2015
#'
#' @seealso \code{\link{obs.wells}}
#'
#' @keywords datasets
#'
#' @examples
#' str(obs.wells.head)
#'
"obs.wells.head"

#' Pilot Points
#'
#' Location of pilot points in the model domain.
#'
#' @format An object of SpatialPointsDataFrame class containing 106 points
#'   with the following variables:
#'   \describe{
#'     \item{Layer}{model layer}
#'     \item{Zone}{kriging-zone identifier}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on June 11, 2015
#'
#' @seealso \code{\link{kriging.zones}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(pilot.points)
#' str(pilot.points@data)
#'
"pilot.points"

#' Points of Diversion for Groundwater
#'
#' Points of diversion for groundwater within the Wood River Valley model study area.
#'
#' @format An object of class data.frame with 1,755 records and the following variables:
#'   \describe{
#'     \item{WMISNumber}{unique number assigned to a water right point of diversion.}
#'     \item{WaterRight}{number identifying a specific authorization to use
#'       water in a prescribed manner.}
#'     \item{EntityName}{name of the irrigation entity the point of diversion is assigned to.}
#'     \item{EntitySrce}{source of water for an irrigation entity.
#'       Possible sources of water include surface water, groundwater and mixed.
#'       Mixed source entities derive water from both groundwater and surface water.}
#'     \item{Pdate}{priority date, the date the water right was established.}
#'     \item{IrrRate}{irrigation rate in cubic meters per day,
#'       the maximum permitted water use rate associated with a water right.}
#'   }
#'
#' @source Idaho Department of Water Resources water rights database,
#'   accessed on December 11, 2014
#'
#' @seealso \code{\link{pod.wells}}
#'
#' @keywords datasets
#'
#' @examples
#' summary(pod.gw)
#'
"pod.gw"

#' Well Completions
#'
#' Well completions for pumping wells in the Wood River Valley aquifer system.
#'
#' @format An object of SpatialPointsDataFrame class containing 1,243 points
#'   with the following variables:
#'   \describe{
#'     \item{WMISNumber}{is a unique number assigned to a water right point of diversion.}
#'     \item{WellUse}{permitted use(s) for a groundwater well.}
#'     \item{TopOpen1}{depth to the top of the first open interval in a groundwater well,
#'       in meters below land surface.}
#'     \item{BotOpen1}{depth to the bottom of the first open interval in a groundwater well,
#'       in meters below land surface.}
#'     \item{TopOpen2}{depth to the top of the second open interval in a groundwater well,
#'       in meters below land surface.}
#'     \item{BotOpen2}{depth to the bottom of the second open interval in a groundwater well,
#'       in meters below land surface.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources water rights database,
#'   accessed on November 29, 2014
#'
#' @seealso \code{\link{pod.gw}}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(pod.wells)
#' str(pod.wells@data)
#'
"pod.wells"

#' Precipitation Zones
#'
#' Precipitation zones specified for the Wood River Valley and surrounding areas.
#' There are three precipitation zones, each containing a single weather station.
#' Precipitation zones were distributed to maintain the geographic similarity between
#' weather stations and zones.
#'
#' @format An object of SpatialPolygonsDataFrame class containing 3 Polygons and a
#'   data.frame with the following variables:
#'   \describe{
#'     \item{ID}{numeric identifier assigned to the polygon.}
#'     \item{PrecipZone}{name of the precipitation zone:
#'       \dQuote{Ketchum}, the northernmost zone with data from the
#'         Ketchum National Weather Service coop weather station.
#'       \dQuote{Hailey}, central zone with data from the
#'         Hailey 3NNW National Weather Service coop weather station.
#'       \dQuote{Picabo}, southernmost zone with data from the
#'         Picabo AgriMet weather station.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Created using the northing midpoint between weather stations,
#'   see \code{\link{weather.stations}} dataset.
#'
#' @seealso \code{\link{precipitation}}
#'
#' @keywords datasets
#'
#' @examples
#' col <- c("#D1F2A5", "#FFC48C", "#F56991")
#' sp::plot(precip.zones, col = col)
#' legend("topright", legend = precip.zones@data$PrecipZone, fill = col, bty = "n")
#' sp::plot(alluvium.extent, add = TRUE)
#'
#' print(precip.zones)
#'
"precip.zones"

#' Precipitation Rate
#'
#' Precipitation rates in the Wood River Valley and surrounding areas.
#'
#' @format An object of data.frame class with 582 records and the following variables:
#'   with the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month during which precipitation were recorded,
#'       with a required date format of \code{YYYYMM}.}
#'     \item{PrecipZone}{name of the precipitation zone,
#'       see \code{\link{precip.zones}} dataset for details.}
#'     \item{Precip}{monthly depth of precipitation accounting for spring melt, in meters.}
#'     \item{Precip.raw}{monthly depth of precipitation recorded at the weather station,
#'       in meters.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on April 24, 2015
#'
#' @references National Oceanic and Atmospheric Administration's National Weather Service
#'   (\href{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/cooperative-observer-network-coop}{NWS}) Cooperative Observer Program
#'
#'   U.S. Bureau of Reclamation's Cooperative Agricultural Weather Network
#'   (\href{https://www.usbr.gov/pn/agrimet/}{AgriMet})
#'
#' @seealso \code{\link{precip.zones}}, \code{\link{swe}}
#'
#' @keywords datasets
#'
#' @examples
#' str(precipitation)
#'
#' d <- precipitation
#' d <- data.frame(Date = as.Date(paste0(d$YearMonth, "15"), format = "\%Y\%m\%d"),
#'                 Precip = d$Precip)
#' zones <- c("Hailey", "Ketchum", "Picabo")
#' d1 <- d[precipitation$PrecipZone == zones[1], ]
#' d2 <- d[precipitation$PrecipZone == zones[2], ]
#' d3 <- d[precipitation$PrecipZone == zones[3], ]
#' d <- merge(merge(d1, d2, by = "Date"), d3, by = "Date")
#'
#' col <- c("red", "blue", "green")
#' ylab <- paste("Precipitation in", c("meters", "feet"))
#' inlmisc::PlotGraph(d, ylab = ylab, col = col, lty = 1:3,
#'                    conversion.factor = 3.28084, seq.date.by = "year")
#' legend("topright", zones, col = col, lty = 1:3, inset = 0.02, cex = 0.7,
#'        box.lty = 1, bg = "#FFFFFFE7")
#'
#' graphics.off()
#'
"precipitation"

#' Priority Cuts
#'
#' Priority cut dates applied to Big Wood River above Magic Reservoir and
#' Silver Creek by Water District 37 and 37M at the end of each month.
#'
#' @format An object of data.frame class with 112 records
#'   and the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month during of the priority cut date,
#'       with a required date format of YYYYMM.}
#'     \item{Pdate_BWR}{date of the priority cut applied to
#'       Big Wood River above Magic Reservoir by Water District 37.}
#'     \item{Pdate_SC}{date of the priority cut applied to
#'       Silver Creek by Water District 37M.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on November 17, 2014;
#'   compiled priority cut dates in effect at the end of each month,
#'   derived from Water District 37 and 37M records.
#'
#' @keywords datasets
#'
#' @examples
#' str(priority.cuts)
#'
"priority.cuts"

#' Public Land Parcels
#'
#' Non-irrigated public land parcels in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialPolygons class containing 669 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on November 29, 2014;
#'   derived from Blaine County tax lots and aerial photography.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(public.parcels)
#' print(public.parcels)
#'
"public.parcels"

#' Rasterized Canals
#'
#' Canal systems of the Wood River Valley and surrounding areas transferred to raster cells.
#'
#' @format An object of RasterLayer class with indexed cell values linked to
#'   a raster attribute table (RAT).
#'   The RAT is a \code{data.frame} with the following components:
#'   \describe{
#'     \item{ID}{cell index}
#'     \item{COUNT}{frequency of the cell index in the raster grid.}
#'     \item{EntityName}{name of the irrigation entity served by the canal system.}
#'   }
#'
#' @source Calculated by transferring the \code{\link{canals}} dataset to grid cells in the
#'   \code{\link{land.surface}} dataset using the \code{rasterize} function;
#'   see the appendix C. Package Dataset Creation for the \R code used in this calculation.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(r.canals)
#' print(levels(r.canals)[[1]])
#'
"r.canals"

#' Stream-Aquifer Flow Exchange Along River Reaches
#'
#' Stream-aquifer flow exchange along river reaches specified as aquifer recharge.
#' Values used as observations in parameter estimation.
#'
#' @format An object of \code{data.frame} class with 192 records and the following variables:
#'   \describe{
#'     \item{YearMonth}{year and month of the measurement record,
#'       with a required date format of \code{YYYYMM}.}
#'     \item{nKet_Hai}{stream-aquifer flow exchange in the Big Wood River,
#'       near Ketchum to Hailey river reach, in cubic meters per day.}
#'     \item{Hai_StC}{stream-aquifer flow exchange in the Big Wood River,
#'       Hailey to Stanton Crossing river reach, in cubic meters per day.}
#'     \item{WillowCr}{stream-aquifer flow exchange in the Willow Creek river reach,
#'       in cubic meters per day.}
#'     \item{SilverAbv}{stream-aquifer flow exchange in Silver Creek,
#'       above Sportsman Access river reach, in cubic meters per day.}
#'     \item{SilverBlw}{stream-aquifer flow exchange in Silver Creek,
#'       Sportsman Access to near Picabo river reach, in cubic meters per day.}
#'   }
#'
#' @details A positive stream-aquifer flow exchange indicates aquifer recharge
#'   (a losing river reach).
#'
#' @source Calculated from continuous stream flow measurements, diversion data,
#'   return flow data, and exchange well data using a flow difference method to
#'   estimate groundwater inflows and outflows along a river reach,
#'   accessed on September 1, 2015.
#'   Derived from U.S. Geological Survey, Idaho Power Company,
#'   and Water District 37 and 37M records.
#'
#' @keywords datasets
#'
#' @examples
#' str(reach.recharge)
#'
"reach.recharge"

#' Major River Reaches
#'
#' Major river reaches of the Wood River Valley, Idaho.
#'
#' @format An object of SpatialLinesDataFrame class containing 22 Lines and a
#'   data.frame with the following variables:
#'   \describe{
#'     \item{Reach}{name of the subreaches measured in U.S. Geological Survey (USGS)
#'       seepage survey.}
#'     \item{BigReach}{name of the reaches for which time series targets are available for
#'       part or all of the calibration period.}
#'     \item{DrainRiver}{model boundary assignment, either \dQuote{drain} or \dQuote{river}.}
#'     \item{RchAvg}{estimated average reach gain in cubic meters per day for 1995-2010
#'       based on a combination of gage data and the USGS seepage survey.}
#'     \item{BigRAv}{estimated average reach gain in cubic meters per day for 1995-2010
#'       based on gage data.}
#'     \item{ReachNo}{reach number identifier.}
#'     \item{Depth}{estimated average depth in meters of water in reach,
#'       measured from the air-water interface to the top of the riverbed sediments.}
#'     \item{BedThk}{estimated thickness in meters of the saturated riverbed sediments.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on June 6, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(river.reaches)
#' str(river.reaches@data)
#'
"river.reaches"

#' Rasterized Monthly Irrigation Entities
#'
#' Irrigation entities of the Wood River Valley and surrounding areas transferred to raster cells.
#'
#' @format An object of RasterStack class containing a 192 RasterLayer objects,
#'   one layer for each month in the 1995-2010 time period.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'   For each raster layer, indexed cell values are linked to a raster attribute table (RAT).
#'   The RAT is an object of data.frame class with the following components:
#'   \describe{
#'     \item{ID}{cell index}
#'     \item{COUNT}{frequency of the cell index in the raster grid.}
#'     \item{EntityName}{name of the irrigation entity served by a group of diversions.}
#'   }
#'
#' @source Calculated by transferring the \code{\link{entity.components}} dataset to grid cells
#'   in the \code{\link{land.surface}} dataset using the rasterize function;
#'   see the appendix C. Package Dataset Creation for the \R code used in this calculation.
#'
#' @keywords datasets
#'
#' @examples
#' names(rs.entities)
#' sp::plot(rs.entities[["199507"]])
#' print(levels(rs.entities[["199507"]])[[1]])
#'
"rs.entities"

#' Rasterized Monthly Recharge on Non-Irrigated Lands
#'
#' Aerial recharge and discharge on non-irrigated lands of the Wood River Valley and
#' surrounding areas transferred to raster cells.
#'
#' @format An object of RasterStack class containing 192 RasterLayer objects,
#'   one layer for each month in the 1995-2010 time period.
#'   Each cell on a layers surface grid represents the monthly recharge in meters.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Calculated from the \code{\link{et}}, \code{\link{precipitation}},
#'   \code{\link{precip.zones}}, and \code{\link{soils}} datasets;
#'   see the appendix C. Package Dataset Creation for the \R code used in this calculation.
#'
#' @keywords datasets
#'
#' @examples
#' names(rs.rech.non.irr)
#' sp::plot(rs.rech.non.irr[["199507"]])
#'
"rs.rech.non.irr"

#' Stream Seepage Study
#'
#' A Wood River Valley stream seepage study with streamflow measurements made during
#' the months of August 2012, October 2012, and March 2013.
#'
#' @format An object of SpatialPointsDataFrame class containing 82 points
#'   with the following variables:
#'   \describe{
#'     \item{Order}{index used to preserve the downstream order of measurement sites.}
#'     \item{Name}{local name for the measurement site.}
#'     \item{SiteNo}{unique identifier for the measurement site within the
#'       National Water Information System (NWIS).}
#'     \item{Type}{the type of measurement site:
#'       \dQuote{Big Wood River}, \dQuote{Willow Creek}, \dQuote{Spring fed creeks},
#'       \dQuote{Silver Creek}, \dQuote{Diversion}, \dQuote{Exchange well inflow},
#'       \dQuote{Return}, and \dQuote{Tributary}.}
#'     \item{Comments}{abbreviated site name}
#'     \item{Aug}{volumetric flow rate measured during August 2012, in cubic meters per day.}
#'     \item{Oct}{volumetric flow rate measured during October 2012, in cubic meters per day.}
#'     \item{Mar}{volumetric flow rate measured during March 2013, in cubic meters per day.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Derived from Bartolino (2014) seepage study,
#'   Idaho Department of Water Resources, Water District 37 and 37M flow records.
#'
#' @references Bartolino, J.R., 2014, Stream seepage and groundwater levels, Wood River Valley,
#'   south-central Idaho, 2012--13: U.S. Geological Survey Scientific Investigations Report 2014-5151,
#'   34 p., \url{https://doi.org/10.3133/sir20145151}.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(seepage.study)
#' str(seepage.study@data)
#'
"seepage.study"

#' PEST Sensitivity
#'
#' Calibrated parameter values and composite sensitivities generated by PEST.
#'
#' @format An object of data.frame class with 336 records and the following variables:
#'   \describe{
#'     \item{parameter.desc}{parameter description}
#'     \item{ID}{unique identifier within the parameter type,
#'       such as an identifier for a pilot point or irrigation entity.}
#'     \item{units}{parameter units}
#'     \item{start.value}{starting parameter value prior to model calibration.}
#'     \item{lower.bound}{lower bound placed on the parameter value
#'       during the model-calibration process.}
#'     \item{upper.bound}{upper bound placed on the parameter value
#'       during the model-calibration process.}
#'     \item{parameter.name}{\href{http://www.pesthomepage.org/}{PEST} parameter name}
#'     \item{group}{PEST parameter group}
#'     \item{value}{calibrated parameter value estimated by PEST.}
#'     \item{comp.sens}{composite sensitivity generated during the final iteration of PEST.}
#'     \item{rel.comp.sens}{relative composite sensitivity}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on January 15, 2016
#'
#' @seealso \code{\link{pilot.points}}, \code{\link{irr.entities}},
#'   \code{\link{river.reaches}}, \code{\link{drains}}, \code{\link{tributaries}}
#'
#' @keywords datasets
#'
#' @examples
#' str(sensitivity)
#'
"sensitivity"

#' Soil Units
#'
#' Representation of mapped surficial soil units created by the Idaho Office of the
#' National Resource Conservation Service (NRCS).
#' Soils have been assigned a percolation rate based on the average,
#' saturated hydraulic conductivity of the soils as classified using the
#' Unified Soil Classification System (USCS).
#'
#' @format An object of SpatialPolygonsDataFrame class containing 718 Polygons and a
#'   data.frame with the following variables:
#'   \describe{
#'     \item{GroupSymbol}{soil class identifier}
#'     \item{SoilLayer}{identifier used to differentiate the soil data source
#'       used to create the soils map.
#'       Data sources are either \sQuote{USCS} or \sQuote{STATSGO},
#'       the NRCS State Soil Geographic Data Base.}
#'     \item{SoilClass}{description of the soil class.}
#'     \item{MinRate}{lower percolation rate limit for the soil class, in meters per month.}
#'     \item{MaxRate}{upper percolation rate limit for the soil class, in meters per month.}
#'     \item{PercolationRate}{percolation rate in meters per month.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on April 22, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::spplot(soils, "PercolationRate")
#' str(soils@data)
#'
"soils"

#' Streamgages
#'
#' Select streamgages in the Wood River Valley.
#'
#' @format An object of SpatialPointsDataFrame class containing 9 points and a
#'   data.frame with the following variable:
#'   \describe{
#'     \item{SiteNo}{unique site number for the streamgage.}
#'     \item{SiteName}{official name of the streamgage.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source National Water Information System (\href{https://waterdata.usgs.gov/nwis}{NWIS}),
#'   accessed on May 29, 2015.
#'
#' @keywords datasets
#'
#' @examples
#' str(streamgages)
#'
"streamgages"

#' Streams and Rivers
#'
#' Streams and rivers of the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialLinesDataFrame class containing 581 Lines.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources
#'   (\href{https://research.idwr.idaho.gov/index.html#GIS-Data}{IDWR}),
#'   accessed on April 2, 2014
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(streams.rivers, col = "#3399CC")
#' str(streams.rivers@data)
#'
"streams.rivers"

#' Stream-Aquifer Flow Exchange Along River Subreaches
#'
#' Stream-aquifer flow exchange along river subreaches specified as aquifer recharge.
#' Values used as observations in parameter estimation.
#'
#' @format An object of data.frame class with 19 records and the following variables:
#'   \describe{
#'     \item{ReachNo}{subreach number identifier}
#'     \item{Reach}{name of the subreach.}
#'     \item{BigReachNo}{reach number identifier}
#'     \item{BigReach}{name of the reach.}
#'     \item{Aug}{estimated volumetric flow rate measured during August 2012,
#'       in cubic meters per day.}
#'     \item{Oct}{estimated volumetric flow rate measured during October 2012,
#'       in cubic meters per day.}
#'     \item{Mar}{estimated volumetric flow rate measured during March 2013,
#'       in cubic meters per day.}
#'   }
#'
#' @details A positive stream-aquifer flow exchange indicates aquifer recharge
#'   (also know as a losing river subreach).
#'
#' @source Flow values calculated from \code{\link{seepage.study}} data.
#'
#' @keywords datasets
#'
#' @examples
#' str(subreach.recharge)
#'
"subreach.recharge"

#' Snow Water Equivalent
#'
#' Average daily snow water equivalent (SWE) at weather stations in the
#' Wood River Valley and surrounding areas.
#'
#' @format An object of data.frame class with 366 records and the following variables:
#'   \describe{
#'     \item{MonthDay}{month and day, with a required date format of \code{MMDD}.}
#'     \item{Choco}{daily SWE recorded at the Chocolate Gulch snow telemetry (SNOTEL)
#'       weather station.}
#'     \item{Hailey}{daily SWE recorded at the Hailey Ranger Station at Hailey
#'       hydrometeorological automated data system (HADS) weather station.}
#'     \item{Picabo}{daily SWE recorded at the Picabo PICI HADS weather station.}
#'   }
#'
#' @source Idaho Department of Water Resources, accessed on April 24, 2015
#'
#' @seealso \code{\link{weather.stations}}, \code{\link{precip.zones}},
#'   \code{\link{precipitation}}
#'
#' @keywords datasets
#'
#' @examples
#' str(swe)
#'
"swe"

#' Tributary Basin Underflow
#'
#' Location and average flow conditions for model boundaries in the
#' major tributary canyons and upper part of the Wood River Valley, south-central Idaho.
#'
#' @format An object of SpatialPolygonsDataFrame class containing a set of 22 Polygons and a
#'   data.frame with the following variable:
#'   \describe{
#'     \item{Name}{tributary name}
#'     \item{MinLSD}{minimum land-surface datum (elevation) along the transect,
#'       in meters above the North American Vertical Datum of 1988 (NAVD 88).}
#'     \item{BdrkDepth}{mean saturated thickness along the transect line, in meters;
#'       estimated as the distance between the estimated water table and bedrock elevations.}
#'     \item{TribWidth}{width of the tributary canyon, or length of the transect line, in meters.}
#'     \item{LandGrad}{land surface elevation gradient perpendicular to the
#'       cross-sectional transect line, a dimensionless quantity.}
#'     \item{K}{hydraulic conductivity in meters per day.}
#'     \item{SatArea}{estimated saturated cross-sectional area, in square meters;
#'       its geometry is represented as the lower-half of an ellipse with
#'       width and height equal to \code{TribWidth} and \code{BdrkDepth}, respectively.}
#'     \item{DarcyFlow}{groundwater flow rate, in cubic meters per day, calculated using a
#'       \href{https://en.wikipedia.org/wiki/Darcy_law}{Darcian} analysis.}
#'     \item{BasinArea}{land-surface area defined by the basin above the cross-sectional transect line.}
#'     \item{BasinAreaType}{label that describes the relative basin size; where
#'       \code{"big"} indicates a basin area greater than 10 square miles (25.9 square kilometers), and
#'       \code{"small"} indicates a basin area that is less than this breakpoint value.}
#'     \item{PrecipRate}{mean precipitation rate within the basin area, in meters per day.}
#'     \item{PrecipFlow}{mean precipitation flow rate, in cubic meters per day,
#'       calculated by multiplying \code{PrecipRate} by \code{BasinArea}.}
#'     \item{FlowRatio}{ratio of darcy flow rate to precipitation flow rate,
#'       or \code{DarcyFlow} divided by \code{PrecipFlow}, a dimensionless quantity.}
#'     \item{Flow}{estimated average volumetric flow rate, in cubic meters per day.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source U.S. Geological Survey, accessed on July 2, 2015;
#'   a Keyhole Markup Language (\href{https://en.wikipedia.org/wiki/Kml}{KML}) file created in
#'   \href{https://www.google.com/earth/}{Google Earth} with polygons drawn by hand in
#'   areas of known specified flow boundaries.
#'   Transect lines are defined by the polygon boundaries within the extent of
#'   alluvium aquifer (see \code{\link{alluvium.extent}} dataset).
#'   The land surface gradient (\code{LandGrad}) was estimated from the
#'   \code{\link{land.surface}} dataset.
#'   Hydraulic conductivity (\code{K}) is the average of two geometric means of hydraulic conductivity
#'   in the unconfined aquifer taken from table 2 in Bartolino and Adkins (2012).
#'   The U.S. Geologic Survey \href{https://water.usgs.gov/osw/streamstats/}{StreamStats} tool
#'   (Ries and others, 2004) was used to delineate the basin area (\code{BasinArea}) and
#'   estimate the precipitation rate (\code{PrecipRate}).
#'   See the appendix C. Package Dataset Creation for the \R code used to calculate the
#'   flow estimates (\code{Flow}).
#'
#' @references Bartolino, J.R., and Adkins, C.B., 2012, Hydrogeologic framework of the
#'   Wood River Valley aquifer system, south-central Idaho: U.S. Geological Survey
#'   Scientific Investigations Report 2012-5053, 46 p.,
#'   available at \url{https://pubs.usgs.gov/sir/2012/5053/}.
#'
#'   Ries, K.G., Steeves, P.A., Coles, J.D., Rea, A.H., and Stewart, D.W., 2004,
#'   StreamStats--A U.S. Geological Survey web application for stream information:
#'   U.S. Geological Survey Fact Sheet FS-2004-3115, 4 p., available at
#'   \url{https://pubs.er.usgs.gov/publication/fs20043115}.
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(tributaries, border = "red")
#' sp::plot(alluvium.extent, add = TRUE)
#' str(tributaries@data)
#'
"tributaries"

#' Tributary Streams
#'
#' Tributary streams of the upper Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialLinesDataFrame class containing 88 Lines.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source Idaho Department of Water Resources, accessed on June 1, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(tributary.streams, col = "#3399CC")
#' str(tributary.streams@data)
#'
"tributary.streams"

#' Weather Stations
#'
#' Weather stations in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialPointsDataFrame class containing 5 points
#'   and the following variables:
#'   \describe{
#'     \item{name}{name of the weather station.}
#'     \item{id}{unique identifier for the weather station.}
#'     \item{type}{type of weather stations:
#'       \code{"HADS"}, a Hydrometeorological Automated Data System operated by the
#'         National Weather Service Office of Dissemination;
#'       \code{"AgriMet"}, a satellite-telemetry network of
#'         automated agricultural weather stations operated and
#'         maintained by the Bureau of Reclamation; and
#'       \code{"SNOTEL"}, an automated system of snowpack and
#'         related climate sensors operated by the
#'         Natural Resources Conservation Service.}
#'     \item{organization}{is the managing organization.}
#'     \item{elevation}{is the elevation of the weather station in
#'       meters above the North American Vertical Datum of 1988 (NAVD 88).}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source National Oceanic and Atmospheric Administration (NOAA),
#'   Bureau of Reclamation, Natural Resources Conservation Service (NRCS),
#'   accessed on May 1, 2015
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(alluvium.extent)
#' sp::plot(weather.stations, col = "red", add = TRUE)
#' str(weather.stations@data)
#'
"weather.stations"

#' Wetlands
#'
#' Wetlands in the Wood River Valley and surrounding areas.
#'
#' @format An object of SpatialPolygons class containing 3,024 Polygons.
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source U.S. Fish and Wildlife Service National Wetlands Inventory,
#'   accessed on April 2, 2014
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(wetlands, col = "#CCFFFF", border = "#3399CC", lwd = 0.5)
#' print(wetlands)
#'
"wetlands"

#' Groundwater-Level Contours for October 2006
#'
#' Groundwater-level contours with a 20 foot (6.096 meter) contour interval for the
#' unconfined aquifer in the Wood River Valley, south-central Idaho,
#' representing conditions during October 2006.
#'
#' @format An object of SpatialLinesDataFrame class containing 265 Lines and a
#'   data.frame with the following variables:
#'   \describe{
#'     \item{CONTOUR}{groundwater elevation contour value in meters above the
#'       North American Vertical Datum of 1988 (NAVD 88).}
#'     \item{certainty}{certainty of the groundwater-level contour based on
#'       data position and density, specified as \code{"good"} or \code{"poor"}.}
#'   }
#'   Geographic coordinates are in units of meters, in conformance with the
#'   North American Datum of 1983 (NAD 83), and placed in the
#'   Idaho Transverse Mercator projection (\href{https://www.idwr.idaho.gov/GIS/IDTM/}{IDTM}).
#'
#' @source This dataset is from Plate 1 in Skinner and others (2007), and available on the
#'   \href{https://water.usgs.gov/GIS/metadata/usgswrd/XML/sir2007-5258_oct2006wl.xml}{WRD NSDI Node}.
#'
#' @references Skinner, K.D., Bartolino, J.R., and Tranmer, A.W., 2007,
#'   Water-resource trends and comparisons between partial development and
#'   October 2006 hydrologic conditions, Wood River Valley, south-central, Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2007-5258, 30 p.,
#'   available at \url{https://pubs.usgs.gov/sir/2007/5258/}
#'
#' @keywords datasets
#'
#' @examples
#' is.good <- wl.200610@data$certainty == "good"
#' sp::plot(wl.200610[is.good, ], col = "blue")
#' sp::plot(wl.200610[!is.good, ], col = "red", lty = 2, add = TRUE)
#' str(wl.200610@data)
#'
"wl.200610"

#' Hydraulic Properties of Hydrogeologic Zones
#'
#' Estimates of the hydraulic properties for each hydrogeologic zone.
#'
#' @format An object of \code{data.frame} class with the following variables:
#'   \describe{
#'     \item{ID}{numeric identifier for the hydrogeologic zone.}
#'     \item{name}{name of the hydrogeologic zone.}
#'     \item{vani}{vertical anisotropy, a dimensionless quantity.}
#'     \item{sc}{storage coefficient, a dimensionless quantity.}
#'     \item{sy}{specific yield, a dimensionless quantity.}
#'     \item{hk}{horizontal hydraulic conductivity in meters per day.}
#'     \item{ss}{specific storage in inverse meter.}
#'   }
#'
#' @references Bartolino, J.R., and Adkins, C.B., 2012, Hydrogeologic framework of the
#'   Wood River Valley aquifer system, south-central Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2012-5053, 46 p.,
#'   available at \url{https://pubs.usgs.gov/sir/2012/5053/}.
#'
#' @keywords datasets
#'
#' @examples
#' str(zone.properties)
#'
"zone.properties"
