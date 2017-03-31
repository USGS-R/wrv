# Unprocessed Data

The *unprocessed* descriptor for data residing in this folder is a relative term;
processing occurs in stages, and the "processed data" from one stage may be considered the "unprocessed data" of the next ([credit](http://en.wikipedia.org/wiki/Data)).
Many of these datasets are composed entirely of data collected from the study site;
however, a few of the datasets are the result of a preliminary stage of data processing.
For example, evapotranspiration and precipitation datasets are the result of processing that is not explicitly shown in the **wrv** package.
The only unprocessed dataset used during model construction and not included under this folder is located on the
[The National Map](http://nationalmap.gov/) and represents the land surface elevation within the study area.

## File and Folder Structure

```
.
+-- canal
|   +-- bypass.canal.op.csv (periods when Big Wood River water was diverted to Bypass Canal)
|   +-- bypass.canal.zip (location of Bypass Canal, diverts water from the Big Wood River)
|   +-- canal.seep.csv (canal seepage as a fraction of diversions for irrigation entities)
|   +-- canals.zip (location of canals)
+-- decorative (not used in model processing)
|   +-- cities.zip
|   +-- idaho.zip
|   +-- lakes.zip
|   +-- major.roads.zip
|   +-- map.labels.csv
|   +-- misc.locations.csv
|   +-- rivers.zip
|   +-- tributary.streams.zip
+-- div (anthropogenic groundwater and surface-water diversions)
|   +-- comb.sw.irr.csv (supplemental groundwater rights and associated surface-water rights)
|   +-- div.gw.csv (groundwater diversions)
|   +-- div.sw.csv (surface-water diversions)
|   +-- div.ww.csv (discharge from wastewater treatment plants)
|   +-- pod.gw.csv (points of diversion for groundwater)
|   +-- pod.wells.zip (well sites)
|   +-- priority.cuts.csv (priority cuts)
+-- et (evapotranspiration)
|   +-- et.199501.tif (mean rates of evapotranspiration for Jan 1995)
|   +-- et.199502.tif (mean rates of evapotranspiration for Feb 1995)
|   +-- ...
|   +-- et.201011.tif (mean rates of evapotranspiration for Nov 2010)
|   +-- et.201012.tif (mean rates of evapotranspiration for Dec 2010)
|   +-- et.method.csv (methods used to calculate evapotranspiration values)
+-- extent
|   +-- alluvium.extent.zip (horizontal extent of alluvium hydrogeologic unit)
|   +-- basalt.extent.zip (horizontal extent of basalt hydrogeologic unit)
|   +-- clay.extent.zip (horizontal extent of clay hydrogeologic unit)
+-- gage (streamgage)
|   +-- gage.13135500.disch.csv (daily mean discharge at near Ketchum gage)
|   +-- gage.13135500.height.csv (daily mean gage height at near Ketchum gage)
|   +-- gage.13139510.disch.csv (daily mean discharge at Hailey gage)
|   +-- gage.13139510.height.csv (daily mean gage height at Hailey gage)
|   +-- gage.13140800.disch.csv (daily mean discharge at Stanton Crossing gage)
|   +-- gage.13140800.height.csv (daily mean gage height at Stanton Crossing gage)
|   +-- streamgages.zip (location of streamgages)
+-- irr (irrigation)
|   +-- efficiency.csv (irrigation efficiency for irrigation entities)
|   +-- irr.entities.zip (location of irrigation entities)
|   +-- irr.lands.1996.zip (location of irrigated lands during 1996)
|   +-- irr.lands.2000.zip (location of irrigated lands during 2000)
|   +-- irr.lands.2002.zip (location of irrigated lands during 2002)
|   +-- irr.lands.2006.zip (location of irrigated lands during 2006)
|   +-- irr.lands.2008.zip (location of irrigated lands during 2008)
|   +-- irr.lands.2009.zip (location of irrigated lands during 2009)
|   +-- irr.lands.2010.zip (location of irrigated lands during 2010)
|   +-- irr.lands.2011.zip (location of irrigated lands during 2011)
|   +-- irr.lands.year.csv (substitute years for missing years of irrigated lands)
+-- opt (parameter estimation)
|   +-- div.ret.exch.zip (diversions, returns, and exchange wells)
|   +-- kriging.zones.zip (kriging zones)
|   +-- obs.wells.head.csv (groundwater-level measurements)
|   +-- obs.wells.zip (observation wells)
|   +-- pilot.points.zip (location of pilot points)
|   +-- reach.recharge.csv (recharge from stream-aquifer flow exchange on major river reaches)
|   +-- seepage.study.zip (stream seepage study)
|   +-- sensitivity.csv (sensitivity analysis)
|   +-- subreach.recharge.csv (recharge from stream-aquifer flow exchange sub-reaches)
+-- precip
|   +-- precip.csv (precipitation recorded at weather stations)
|   +-- swe.choco.csv (snow water equivalent recorded at the Chocolate Gulch weather station)
|   +-- swe.hailey.csv (snow water equivalent recorded at the Hailey weather station)
|   +-- swe.picabo.csv (snow water equivalent recorded at the Picabo weather station)
|   +-- weather.stations.csv (location of weather stations)
+-- alluvium.thickness.tif (estimated thickness of the Quaternary sediment)
+-- bellevue.wwtp.ponds.zip (location of Bellevue Waste Water Treatment Plant ponds)
+-- drains.csv (drain properties)
+-- drains.kml (location of outlet boundaries represented as drains)
+-- misc.seepage.csv (recharge rates at miscellaneous recharge sources)
+-- perennial.reaches.csv (perennial river reaches)
+-- public.parcels.zip (location of public land parcels)
+-- README.md
+-- river.reaches.zip (location of major river reaches)
+-- soils.csv (maximum seepage rates on surficial soil units)
+-- soils.zip (location of surficial soil units)
+-- tributaries.csv (flow properties in the tributaries)
+-- tributaries.kml (location of groundwater inflow boundaries in the tributary canyons)
+-- wetlands.zip (location of wetlands)
+-- wl.200610.zip (groundwater-level contours for October 2006)
+-- zone.properties.csv (properties of the hydrogeologic zones)
```

## File Formats

|Extension |Type   |Description                                  |
|:---------|:------|:--------------------------------------------|
|.csv      |text   |Comma-Separated Values                       |
|.kml      |text   |Keyhole Markup Language                      |
|.md       |text   |Markdown                                     |
|.tif      |binary |Geo-referenced Tagged Image File Format      |
|.zip      |binary |Compressed Point, Line, or Polygon Shapefile |
